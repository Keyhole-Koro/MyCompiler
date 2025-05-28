FROM haskell:9.2.8

WORKDIR /app

# Install build tools early
RUN stack update && stack install alex happy

# Copy your project source
COPY . .

# Build only dependencies first to improve Docker layer caching
RUN stack build --only-dependencies

# Build the actual project
RUN stack build
