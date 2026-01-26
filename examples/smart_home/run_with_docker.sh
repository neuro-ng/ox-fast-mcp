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
cleanup

echo "Starting diyhue container..."
# Assuming diyhue/core:latest is available or pulled
# Mapping port 80 to 8080 (as per test expectations 127.0.0.1:8080)
# Note: DIYHue usually runs on port 80 inside the container.
docker run -d \
  --name $CONTAINER_NAME \
  -p 8080:80 \
  -e MAC=00:11:22:33:44:55 \
  -v $(pwd)/test_config.json:/opt/hue-emulator/config.json \
  diyhue/core:latest

echo "Waiting for diyhue to be ready..."
# Simple health check loop
max_retries=30
count=0
while ! curl -s http://127.0.0.1:8080/api/config >/dev/null; do
  sleep 1
  count=$((count+1))
  if [ $count -ge $max_retries ]; then
    echo "Timeout waiting for diyhue to start"
    exit 1
  fi
  echo -n "."
done
echo " Ready!"

echo "Running tests..."
CMD="$1"
shift
if [[ "$CMD" != /* && "$CMD" != ./* ]]; then
  CMD="./$CMD"
fi
$CMD "$@"
