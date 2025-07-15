FROM ubuntu:latest
RUN apt-get update && apt-get install -y gnucobol
COPY . /app
WORKDIR /app
RUN cobc -x XRPPRocessor.cbl
CMD ["./XRPPRocessor"]
