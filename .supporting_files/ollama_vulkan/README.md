# Ollama (Vulkan) Container

Ollama built to run on this laptop's **Intel Arc B390** integrated GPU
(Panther Lake / Xe3) via the **Vulkan** backend.

Used by `.config/containers/systemd/ollama-mob-zach.container`, which references
the image by name and therefore requires it to be built locally first.

## Why not an off-the-shelf image?

| Image | Result on this GPU |
|---|---|
| `ollama/ollama:latest` | CPU + CUDA backends only — no Intel support |
| `ollama/ollama:rocm` | AMD only; needs `/dev/kfd`, which does not exist here |
| `intelanalytics/ipex-llm-inference-cpp-xpu` | **Silently falls back to CPU** |

The ipex-llm case is the subtle one, and the reason this directory exists. That
image uses oneAPI/SYCL, which reaches the GPU through Level Zero — but it pins
`intel-level-zero-gpu 1.6.32224.5` and `intel-opencl-icd 24.52`, both dated
January 2025, which predate Xe3 silicon. Inside that container `sycl-ls` reports
only the CPU, and ollama logs:

```
msg="using Intel GPU"
msg="inference compute" id=0 library=cpu ... total="62.3 GiB"
```

Note it announces `using Intel GPU` and *then* registers `library=cpu`. The
first line only describes the build flavour, not a successful detection — so
this failure looks like success unless you read the second line. Upstream has no
fix available: `latest` and `2.3.0-SNAPSHOT` were both built 2025-08-26.

Homebrew's host-native `ollama` fails for an unrelated reason — its lib
directory contains only `libggml-cpu-*.so`, with no GPU backend compiled in,
so its `OLLAMA_VULKAN:true` default has nothing to load.

## How this image works

Two pieces have to line up:

1. **Upstream ollama already ships a Vulkan backend** — the release bundle
   contains `lib/ollama/vulkan/libggml-vulkan.so`. No source build is needed.
2. **Mesa must be new enough for Xe3.** The base is Fedora (Mesa 26.x) for
   exactly this reason. Ubuntu 24.04 ships Mesa 24.x and would reproduce the
   same too-old-userspace failure as ipex-llm.

Two environment variables are baked in because they are properties of *this
GPU* rather than of the deployment:

- `OLLAMA_IGPU_ENABLE=1` — the B390 is an **integrated** GPU. Ollama discovers
  it and then discards it (`dropping integrated GPU; to enable, set
  OLLAMA_IGPU_ENABLE=1`). Without this the container detects the GPU and still
  runs on CPU.
- `GGML_VK_VISIBLE_DEVICES=0` — Mesa also exposes `llvmpipe`, a software
  rasterizer, as a Vulkan device. Pinning device 0 prevents ollama from landing
  on it, which would report GPU offload while running at CPU speed.

The CUDA backends are deleted during the build; they are roughly 1 GB of dead
weight on this machine.

## Building

```bash
just build-ollama-vulkan
```

Or directly:

```bash
podman build -t localhost/ollama-vulkan:latest .
```

Pin a different ollama release with `--build-arg OLLAMA_VERSION=v0.33.1`.

## Verifying GPU offload

Device detection — look for `library=Vulkan`, not `library=cpu`:

```bash
podman run --rm --device /dev/dri --entrypoint bash localhost/ollama-vulkan:latest \
    -c 'ollama serve 2>&1 | grep "inference compute"'
```

Expected:

```
inference compute id=0 library=Vulkan name=Vulkan0 \
  description="Intel(R) Arc(tm) B390 (PTL)" type=iGPU total="46.7 GiB"
```

Detection alone is not proof of offload. To confirm layers actually land on the
GPU, run a model and check for `offloaded N/N layers to GPU`:

```bash
podman run --rm --device /dev/dri --shm-size=8g \
    -v "${HOME}/.ollama:/root/.ollama:z" --entrypoint bash \
    localhost/ollama-vulkan:latest -c \
    'ollama serve > /tmp/s.log 2>&1 & sleep 5
     ollama run qwen2.5:0.5b "hi" >/dev/null 2>&1
     grep -E "offloaded|Vulkan0 model buffer" /tmp/s.log'
```

Verified on 2026-08-27 with ollama v0.33.1 and Mesa 26.1.8:
`offloaded 25/25 layers to GPU`, `Vulkan0 model buffer size = 373.71 MiB`.

## Notes and pitfalls

- **`ENTRYPOINT` is `/usr/bin/ollama`**, so `podman run ... bash` is parsed as
  an ollama subcommand. Use `--entrypoint bash` for a shell.
- **`total=46.7 GiB` is shared system RAM**, not dedicated VRAM — this is an
  iGPU. Large models will contend with host memory, so `OLLAMA_MAX_LOADED_MODELS`
  in the quadlet is the knob that matters most here.
- The image is **not** pushed to a registry. `podman auto-update` cannot manage
  it; rebuild manually after bumping `OLLAMA_VERSION`.
- If a future Mesa or ollama release regresses, the fastest triage is
  `vulkaninfo --summary` inside the container — if `Intel(R) Arc(tm) B390 (PTL)`
  is absent there, the problem is Mesa, not ollama.
