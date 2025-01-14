# Build stage
FROM elixir:1.14-alpine AS builder

# Install build dependencies
RUN apk add --no-cache build-base git

# Create build directory
WORKDIR /app

# Create hex user and set permissions
RUN adduser -D hex && \
    chown -R hex:hex /app

# Switch to hex user for build
USER hex

# Install hex and rebar
RUN mix local.hex --force && \
    mix local.rebar --force

# Copy mix files
COPY --chown=hex:hex mix.exs mix.lock ./
COPY --chown=hex:hex config config

# Create build directory structure
RUN mkdir -p _build/prod/lib/hex

# Install dependencies
RUN mix deps.get --only prod

# Copy source code
COPY --chown=hex:hex lib lib
COPY --chown=hex:hex src src

# Set environment to production
ENV MIX_ENV=prod

# Compile the application
RUN mix compile

# Runtime stage
FROM elixir:1.14-alpine

# Install runtime dependencies
RUN apk add --no-cache openssl

# Create app directory
WORKDIR /app

# Create hex user
RUN adduser -D hex && \
    chown -R hex:hex /app

# Copy built application
COPY --from=builder --chown=hex:hex /app/_build /app/_build
COPY --from=builder --chown=hex:hex /app/deps /app/deps
COPY --from=builder --chown=hex:hex /app/lib /app/lib
COPY --from=builder --chown=hex:hex /app/src /app/src
COPY --from=builder --chown=hex:hex /app/mix.exs /app/mix.exs

# Switch to hex user
USER hex

# Set environment to production
ENV MIX_ENV=prod

# Command to run the application
CMD ["mix", "run", "--no-halt"]
