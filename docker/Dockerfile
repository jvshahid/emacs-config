FROM jvshahid/emacs
MAINTAINER John Shahid
RUN apt-get update && \
    apt-get -y install \
                silversearcher-ag \
                ispell \
                libgmime-2.6-dev \
                libxapian-dev \
                libxml2-dev \
                libgtkextra-dev \
                libavfilter-dev \
                libao-dev \
                libjson-c-dev && \
     rm -rf /var/lib/apt/lists
