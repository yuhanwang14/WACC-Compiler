FROM ubuntu:latest

# Set up environment variables
ENV DEBIAN_FRONTEND=noninteractive

# Install necessary dependencies
RUN apt-get update && apt-get install -y \
    qemu-system-arm qemu-user gcc-aarch64-linux-gnu binutils-aarch64-linux-gnu \
    make cmake python3 openjdk-11-jdk scala curl gnupg2 wget

# Add SBT repository and install it properly
RUN curl -fsSL https://scala.jfrog.io/artifactory/api/gpg/key/public | gpg --dearmor | tee /usr/share/keyrings/scala-keyring.gpg > /dev/null && \
    echo "deb [signed-by=/usr/share/keyrings/scala-keyring.gpg] https://scala.jfrog.io/artifactory/debian all main" | tee /etc/apt/sources.list.d/scala.list && \
    apt-get update && apt-get install -y sbt

# Set up workspace
WORKDIR /workspace
