FROM ci-build AS builder

FROM cypress/included:4.0.0
RUN npm install -g local-web-server
COPY --from=builder /app /app

WORKDIR /app
ENTRYPOINT ./ci-integration-tests.sh
