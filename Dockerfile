FROM debian:bullseye-slim

ENV LANG=C.UTF-8
RUN useradd -m bot
ADD ./advent-calendar-bot-Linux.tar.gz /home/bot
RUN ln -s /home/bot/advent-calendar-bot/advent-calendar-bot-exe /usr/local/bin/advent-calendar-bot-exe

USER bot
WORKDIR /home/bot
