# SPDX-License-Identifier: PMPL-1.0-or-later
# Multi-stage Dockerfile for Oblíbený with crypto libraries

# Stage 1: Build OCaml compiler and crypto libraries
FROM fedora:40 AS builder

# Install build dependencies
RUN dnf install -y \
    ocaml ocaml-dune ocaml-menhir ocaml-sedlex-devel \
    ocaml-yojson-devel ocaml-ppx-deriving-devel \
    gcc make cmake ninja-build git \
    libsodium-devel openssl-devel \
    && dnf clean all

# Install liboqs (post-quantum crypto)
WORKDIR /tmp
RUN git clone --depth 1 https://github.com/open-quantum-safe/liboqs.git && \
    cd liboqs && \
    mkdir build && cd build && \
    cmake -GNinja -DCMAKE_INSTALL_PREFIX=/usr/local .. && \
    ninja && ninja install

# Install Zig for FFI
RUN curl -L https://ziglang.org/download/0.13.0/zig-linux-x86_64-0.13.0.tar.xz | tar xJ && \
    mv zig-linux-x86_64-0.13.0 /usr/local/zig && \
    ln -s /usr/local/zig/zig /usr/local/bin/zig

# Copy source code
WORKDIR /build
COPY . .

# Build Zig FFI
WORKDIR /build/ffi/zig
RUN zig build -Doptimize=ReleaseFast

# Build Oblíbený compiler and tools
WORKDIR /build
RUN dune build --release && \
    dune install --prefix=/usr/local

# Stage 2: Runtime image
FROM fedora:40-minimal

# Install runtime dependencies only
RUN microdnf install -y \
    libsodium openssl-libs \
    && microdnf clean all

# Copy binaries and libraries from builder
COPY --from=builder /usr/local/lib/liboqs.so* /usr/local/lib/
COPY --from=builder /usr/local/bin/oblibeny /usr/local/bin/
COPY --from=builder /usr/local/bin/oblibeny-lsp /usr/local/bin/

# Set library path
ENV LD_LIBRARY_PATH=/usr/local/lib:$LD_LIBRARY_PATH

# Create non-root user
RUN useradd -m -s /bin/bash oblibeny
USER oblibeny
WORKDIR /home/oblibeny

# Health check
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
  CMD oblibeny --version || exit 1

# Default command
ENTRYPOINT ["oblibeny"]
CMD ["--help"]
