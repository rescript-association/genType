#!/usr/bin/env bash
# This script is only relevant if you're rolling nixos.

# Esy (a bisect_ppx dependency/build tool) is borked on nixos without using an FHS shell. https://github.com/esy/esy/issues/858
# We need to patchelf rescript executables. https://github.com/NixOS/nixpkgs/issues/107375
set -x

fhsShellName="gentype-development"
fhsShellDotNix="{pkgs ? import <nixpkgs> {} }: (pkgs.buildFHSUserEnv { name = \"${fhsShellName}\"; targetPkgs = pkgs: [pkgs.yarn]; runScript = \"yarn\"; }).env"
nix-shell - <<<"$fhsShellDotNix"

# theLd=$(patchelf --print-interpreter $(which mkdir))
# patchelf --set-interpreter $theLd ./node_modules/gentype/gentype.exe
# patchelf --set-interpreter $theLd ./node_modules/rescript/linux/*.exe
# patchelf --set-interpreter $theLd ./node_modules/bisect_ppx/ppx
# # patchelf --set-interpreter $theLd ./node_modules/bisect_ppx/bisect-ppx-report
# theSo=$(find /nix/store/*$fhsShellName*/lib64 -name libstdc++.so.6 | head -n 1)
# patchelf --replace-needed libstdc++.so.6 $theSo ./node_modules/rescript/linux/ninja.exe
