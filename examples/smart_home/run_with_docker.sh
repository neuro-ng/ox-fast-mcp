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

echo "Starting diyhue container..."
# Using create + cp + start to avoid volume mount issues in CI/DooD environments
# where the build path inside the container doesn't match the host path.
docker create \
  --name $CONTAINER_NAME \
  -p 0:80 \
  -e MAC=00:11:22:33:44:55 \
  diyhue/core:latest

# Copy config file (docker cp works from client to container, bypassing host path issues)
docker cp $(pwd)/test_config.json $CONTAINER_NAME:/opt/hue-emulator/config.json

# Start the container
docker start $CONTAINER_NAME

# Get mapped port
HOST_PORT=$(docker port $CONTAINER_NAME 80 | head -n 1 | awk -F: '{print $2}')
CONTAINER_ADDR="127.0.0.1:$HOST_PORT"
echo "Container Address: $CONTAINER_ADDR"

echo "Waiting for diyhue to be ready..."
# Simple health check loop
max_retries=60
count=0
while ! curl -m 1 -s http://$CONTAINER_ADDR/api/config >/dev/null; do
  sleep 1
  count=$((count+1))
  if [ $count -ge $max_retries ]; then
    echo "Timeout waiting for diyhue to start"
    echo "Container logs:"
    docker logs $CONTAINER_NAME
    exit 1
  fi
  echo -n "."
done
echo " Ready!"

export SMART_HOME_TEST_IP="$CONTAINER_ADDR"
echo "Running tests with SMART_HOME_TEST_IP=$SMART_HOME_TEST_IP"
CMD="$1"
shift
if [[ "$CMD" != /* && "$CMD" != ./* ]]; then
  CMD="./$CMD"
fi
$CMD "$@"
