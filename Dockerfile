FROM ubuntu:latest
RUN apt-get update && apt-get install -y gnucobol
COPY . /app
WORKDIR /app
RUN mkdir /app/data  # Create subdir for persistence
RUN cobc -x -free XRPPRocessor.CBL
CMD ["./XRPPRocessor"]
