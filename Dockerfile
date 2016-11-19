FROM phusion/passenger-customizable

MAINTAINER Christopher A. Mosher <cmosher01@gmail.com>

RUN \
    apt-get update && \
    DEBIAN_FRONTEND=noninteractive apt-get dist-upgrade -y -o Dpkg::Options::="--force-confold" -o Dpkg::Options::="--force-confdef" --no-install-recommends && \
    apt-get autoremove -y && \
    apt-get clean



RUN \
    apt-get update && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y -o Dpkg::Options::="--force-confold" -o Dpkg::Options::="--force-confdef" --no-install-recommends \
        automake autoconf autopoint xa65 asciidoc source-highlight \
    && \
    apt-get autoremove -y && \
    apt-get clean



RUN chmod -R a+rwX /usr/local
RUN ln -s /home/app/apple2/doc /var/www/html/apple2
RUN sed -i "s/worker_processes.*/worker_processes 1;/" /etc/nginx/nginx.conf
RUN rm -f /etc/service/nginx/down



USER app
ENV HOME /home/app
WORKDIR $HOME



USER root
ADD http://mosher.mine.nu/a2catalog/a2catalog-1.2-SNAPSHOT.tar.gz ./
RUN chmod a+r a2catalog-1.2-SNAPSHOT.tar.gz
USER app
RUN tar xzf a2catalog-1.2-SNAPSHOT.tar.gz
RUN cd a2catalog-1.2-SNAPSHOT && ./configure && make && make install && cd -



RUN mkdir apple2
WORKDIR apple2



USER root

COPY bootstrap configure.ac Makefile.am ./

COPY NEWS README* AUTHORS ChangeLog COPYING* ./

COPY src/ ./src/
COPY doc/ ./doc/
RUN chown -R app: *

USER app



COPY bld.sh ./

RUN ./bld.sh ./bootstrap
RUN ./bld.sh ./configure
RUN ./bld.sh make
RUN ./bld.sh make check
RUN ./bld.sh make dist
RUN ./bld.sh make distcheck
RUN ./bld.sh make install
RUN ./bld.sh make installcheck

RUN cd doc && ln -s ../apple2sys-*.tar.gz apple2sys.tar.gz && cd -

USER root
