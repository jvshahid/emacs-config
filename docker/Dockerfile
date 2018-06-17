FROM jvshahid/emacs
MAINTAINER John Shahid
RUN apt-get update && \
    apt-get -y install \
                silversearcher-ag \
                ispell \
                libgmime-2.6-dev \
                libxapian-dev \
                libgtkextra-dev \
                libavfilter-dev \
                libao-dev \
                libjson-c-dev \
                libxtst6 \
                pulseaudio-utils \
                gdb \
                curl && \
     rm -rf /var/lib/apt/lists
COPY pulse-client.conf /etc/pulse/client.conf
