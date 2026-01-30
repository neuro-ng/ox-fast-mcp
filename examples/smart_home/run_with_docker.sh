#!/bin/bash
set -e

# Container name
CONTAINER_NAME="smart-home-test-diyhue"

# Cleanup function
cleanup() {
  echo "Cleaning up..."
  if [ "$(docker ps -q -f name=$CONTAINER_NAME)" ]; then
    docker stop $CONTAINER_NAME >/dev/null
  fi
  if [ "$(docker ps -aq -f name=$CONTAINER_NAME)" ]; then
    docker rm $CONTAINER_NAME >/dev/null
  fi
}

# Trap exit signals
trap cleanup EXIT

# Clean up any existing container
# Clean up any existing container
cleanup

# Configure networking based on environment
if [ "$GITHUB_ACTIONS" = "true" ]; then
  echo "Detected GitHub Actions environment."
  # Get the network of the current container
  RUNNER_NETWORK=$(docker inspect --format '{{range $k, $v := .NetworkSettings.Networks}}{{$k}}{{end}}' $(hostname) || echo "bridge")
  echo "Running on network: $RUNNER_NETWORK"
  
  # Run on the same network, no port mapping needed (access via container name)
  NETWORK_ARGS="--network $RUNNER_NETWORK --network-alias $CONTAINER_NAME"
  HEALTH_URL_BASE="http://$CONTAINER_NAME:80"
  
  # Ensure we don't try to reuse name if it exists (though cleanup handled this)
  
else
  echo "Detected local/dev environment."
  # Standard port mapping for local dev
  NETWORK_ARGS="-p 8080:80"
  HEALTH_URL_BASE="http://127.0.0.1:8080"
fi

echo "Starting diyhue container..."
# Using create + cp + start to avoid volume mount issues in CI/DooD environments
docker create \
  --name $CONTAINER_NAME \
  $NETWORK_ARGS \
  -e MAC=00:11:22:33:44:55 \
  diyhue/core:latest

# Copy config file (docker cp works from client to container, bypassing host path issues)
docker cp $(pwd)/test_config.json $CONTAINER_NAME:/opt/hue-emulator/config.json

# Start the container
docker start $CONTAINER_NAME

echo "Waiting for diyhue to be ready..."
# Get container IP address (works in Docker-in-Docker setups)
CONTAINER_IP=$(docker inspect -f '{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' $CONTAINER_NAME)
echo "Container IP: $CONTAINER_IP"

# Health check loop - try both addresses until one works
echo "Waiting for diyhue to be ready (checking $HEALTH_URL_BASE and IP)..."
target_found=false
max_retries=300
count=0

while [ $count -lt $max_retries ]; do
  # Try the primary URL (DNS or localhost)
  if curl -s --max-time 1 "$HEALTH_URL_BASE/api/config" > /dev/null; then
    echo " Connection successful via $HEALTH_URL_BASE"
    target_found=true
    TARGET_HOST="$HEALTH_URL_BASE"
    # Extract host part for export (remove http:// and /...)
    TARGET_HOST=${TARGET_HOST#http://}
    TARGET_HOST=${TARGET_HOST%%/*}
    break
  fi

  # Try container IP as fallback
  if [ -n "$CONTAINER_IP" ]; then
    if curl -s --max-time 1 "http://$CONTAINER_IP:80/api/config" > /dev/null; then
      echo " Connection successful via Container IP: $CONTAINER_IP"
      target_found=true
      TARGET_HOST="$CONTAINER_IP:80"
      break
    fi
  fi

  sleep 1
  count=$((count+1))
  echo -n "."
done

if [ "$target_found" = false ]; then
  echo "Timeout waiting for diyhue to start"
  echo "Container logs:"
  docker logs $CONTAINER_NAME
  cleanup
  exit 1
fi

echo " Ready!"
export SMART_HOME_TEST_IP="$TARGET_HOST"

echo "Running tests..."
CMD="$1"
shift
if [[ "$CMD" != /* && "$CMD" != ./* ]]; then
  CMD="./$CMD"
fi

# Run test with timeout to prevent indefinite hangs (5 minutes max)
# timeout returns 124 if the command times out
timeout 300 $CMD "$@"
TEST_EXIT_CODE=$?

if [ $TEST_EXIT_CODE -eq 124 ]; then
  echo "ERROR: Test timed out after 5 minutes"
  exit 1
elif [ $TEST_EXIT_CODE -ne 0 ]; then
  echo "Tests failed with exit code: $TEST_EXIT_CODE"
  exit $TEST_EXIT_CODE
else
  echo "Tests completed successfully"
  exit 0
fi
