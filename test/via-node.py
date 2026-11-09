#!/usr/bin/env python3
"""Optional integration test: python3 test/via-node.py HSC2HS EMCC NODE.

Requires POSIX, Python 3, Emscripten and Node.js. No downloads or host ABI assumptions.
"""
import json
import shlex
from pathlib import Path
import subprocess
import sys
import tempfile

hsc, emcc, node = [str(Path(p).resolve()) for p in sys.argv[1:]]
template = str(Path(__file__).resolve().parents[1] / 'data/template-hsc.h')

with tempfile.TemporaryDirectory(prefix='hsc2hs node ') as tmp:
    root = Path(tmp)
    # Exercise CommonJS execution even inside a JavaScript ESM package.
    (root / 'package.json').write_text('{"type":"module"}')
    source = root / 'constants.hsc'
    source.write_text('''#include <stdint.h>
#include <stddef.h>
#include <sys/resource.h>
#define NEGATIVE (-123)
#define TEXT "quoted\\\" and slash\\\\"
#def struct sample { char tag; uint64_t value; };
maximum = #{const UINT64_MAX}
infinity = #{const RLIM_INFINITY}
negative = #{const NEGATIVE}
pointerSize = #{size void *}
longSize = #{size long}
structSize = #{size struct sample}
alignment = #{alignment struct sample}
offset = #{offset struct sample, value}
type Word = #{type uint64_t}
#if __EMSCRIPTEN__
branch = True
#else
#error wrong target
#endif
text = #{const_str TEXT}
#enum Int, , first = 1, last = 17
''')
    base = [hsc, '--template=' + template, '--cc=' + emcc, '--ld=' + emcc]
    def run(args, ok=True):
        result = subprocess.run(args, cwd=root, text=True, stdout=subprocess.PIPE,
                                stderr=subprocess.PIPE, timeout=120)
        assert (result.returncode == 0) == ok, result.stderr
        return result
    def artifacts():
        return list(root.glob('*_hsc_make*')) + list(root.glob('*_hsc_utils*'))

    out = root / 'result with spaces.hs'
    args = base + ['-x', '--via-node=' + node, '-v', '-o', str(out), str(source)]
    result = run(args)
    text = out.read_text()
    for expected in ['maximum = 18446744073709551615',
                     'infinity = 18446744073709551615',
                     'negative = -123', 'pointerSize = (4)', 'longSize = (4)',
                     'structSize = (16)', 'alignment = 8', 'offset = (8)',
                     'type Word = Word64', 'branch = True',
                     'first  =  1', 'last  =  17']:
        assert expected in text, (expected, text)
    assert 'quoted\\" and slash\\\\' in text, text
    # main + utility object + link, regardless of number/magnitude of constants.
    assert result.stderr.count('Executing: (@') == 3, result.stderr
    assert not artifacts(), artifacts()
    assert out.with_name(out.stem + '_hsc.h').exists()
    assert out.with_name(out.stem + '_hsc.c').exists()

    # -x ordering must not change the chosen generator or output.
    run(base + ['--via-node=' + node, '-x', '-o', str(out), str(source)])
    assert out.read_text() == text

    # Keep-files retains one self-contained CJS program, no wasm sidecar.
    run(args + ['-k'])
    assert out.with_name(out.stem + '_hsc_make.cjs').exists()
    assert not list(root.glob('*.wasm'))

    # Native path and compiler-probe path remain available (simple shared input).
    small = root / 'small.hsc'
    small.write_text('value = #{const 42}')  # Deliberately no final newline.
    native = root / 'native.hs'
    run([hsc, '--template=' + template, '-o', str(native), str(small)])
    run(base + ['-x', '-o', str(out), str(small)])
    assert out.read_text() == native.read_text(), (repr(out.read_text()), repr(native.read_text()))
    run(base + ['--via-node=' + node, '-o', str(out), str(small)])
    assert out.read_text() == native.read_text(), (repr(out.read_text()), repr(native.read_text()))

    # A fake Node program verifies real argv (not @response-file) and errors.
    fake = root / 'fake node.cjs'
    fake.write_text('console.error(JSON.stringify(process.argv.slice(2))); process.exit(23);')
    wrapper = root / 'node wrapper'
    wrapper.write_text('#!/bin/sh\nexec ' + shlex.quote(node) + ' ' + shlex.quote(str(fake)) + ' "$@"\n')
    wrapper.chmod(0o755)
    failed = root / 'failed.hs'
    result = run(base + ['--via-node=' + str(wrapper), '-o', str(failed), str(small)], ok=False)
    assert 'exit code 23' in result.stderr, result.stderr
    assert json.dumps([str(root / 'failed_hsc_make.cjs')]) in result.stderr, result.stderr
    assert not list(root.glob('failed_hsc_make*'))
    assert not list(root.glob('failed_hsc_utils*'))

    # --no-compile must not try to execute even an absent compiler/runner.
    run([hsc, '--template=' + template, '--cc=missing-compiler',
         '--via-node=missing-node', '-x', '--no-compile', str(small)])
    assert small.with_name('small_hsc_make.c').exists()
    run(base + ['--via-node=' + node, '--via-asm', str(small)], ok=False)
    run(base + ['--via-node=', str(small)], ok=False)
    # Explicit cross-safe restriction is still enforced in runner mode.
    run(base + ['--via-node=' + node, '--cross-safe', str(source)], ok=False)

print('PASS: batched target constants, wasm ABI, CJS/ESM, paths, cleanup, failures, native/probe modes')
