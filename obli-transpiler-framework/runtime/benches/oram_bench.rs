// SPDX-License-Identifier: MIT OR Palimpsest-0.8
// Copyright (c) 2024 Hyperpolymath

//! ORAM benchmarks

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion};
use oblibeny_runtime::prelude::*;

fn bench_oarray_read(c: &mut Criterion) {
    let mut group = c.benchmark_group("OArray Read");

    for size in [100, 1000, 10000] {
        let mut arr: OArray<u64> = OArray::new(size);

        // Initialize
        for i in 0..size {
            arr.write(i, i * 10);
        }

        group.bench_with_input(BenchmarkId::from_parameter(size), &size, |b, &size| {
            let idx = size / 2;
            b.iter(|| black_box(arr.read(black_box(idx))));
        });
    }

    group.finish();
}

fn bench_oarray_write(c: &mut Criterion) {
    let mut group = c.benchmark_group("OArray Write");

    for size in [100, 1000, 10000] {
        let mut arr: OArray<u64> = OArray::new(size);

        group.bench_with_input(BenchmarkId::from_parameter(size), &size, |b, &size| {
            let idx = size / 2;
            b.iter(|| arr.write(black_box(idx), black_box(12345)));
        });
    }

    group.finish();
}

fn bench_constant_time_ops(c: &mut Criterion) {
    let mut group = c.benchmark_group("Constant Time");

    group.bench_function("cmov u64", |b| {
        b.iter(|| cmov(black_box(true), black_box(42u64), black_box(0u64)));
    });

    group.bench_function("cswap u64", |b| {
        let mut a = 1u64;
        let mut x = 2u64;
        b.iter(|| {
            cswap(black_box(true), &mut a, &mut x);
        });
    });

    let array = [1u64, 2, 3, 4, 5, 6, 7, 8, 9, 10];
    group.bench_function("ct_lookup [10]", |b| {
        b.iter(|| ct_lookup(&array, black_box(5)));
    });

    group.finish();
}

criterion_group!(
    benches,
    bench_oarray_read,
    bench_oarray_write,
    bench_constant_time_ops
);
criterion_main!(benches);
