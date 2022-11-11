FROM debian:bullseye-slim

RUN useradd -m bot

COPY ./advent-calendar-bot-exe /usr/local/bin/

USER bot
WORKDIR /home/bot
