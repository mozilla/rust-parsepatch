import codecs
from collections import OrderedDict
import json
import os
import re
import whatthepatch


def _bytes_repr(c):
    # https://github.com/jboy/distil/blob/master/distil/unicode_string_utils.py#L170
    if c == 146:
        return u'\u2019'
    if c == 147:
        return u'\u201C'
    if c == 150:
        return u'\u2013'
    raise UnicodeDecodeError()


def myreplacement(ex):
    s, start, end = ex.object, ex.start, ex.end
    return ''.join(_bytes_repr(c) for c in s[start:end]), end


codecs.register_error('myreplacement', myreplacement)


pat = re.compile(r"\t+$", re.MULTILINE)
directory = './patches'
for f in os.listdir(directory):
    if not f.endswith('.patch'):
        continue
    with open(os.path.join(directory, f), 'rb') as In:
        patch = In.read()

    patch = patch.decode('utf-8', errors='myreplacement')
    bp = 'third_party/rust/bitvec/doc/Bit Patterns.md'
    rep = 'third_party/rust/bitvec/doc/BitPatterns.md'
    if bp in patch:
        patch = patch.replace(bp, rep)
        patch = pat.sub('', patch)
    res = []

    if 'd12b0a6c7641' in f:
        # need to hack here because of bug in wtp
        patch = patch.replace('diff commits', 'Diff commits')
        patch = patch.replace('diffs for', 'Diffs for')

    for diff in whatthepatch.parse_patch(patch):
        r = OrderedDict()

        old = diff.header.old_path
        new = diff.header.new_path

        old = old[2:] if old.startswith('a/') else old
        new = new[2:] if new.startswith('b/') else new

        if old == rep:
            old = bp

        if new == rep:
            new = bp

        r['filename'] = new
        r['new'] = False
        r['deleted'] = False
        r['binary'] = False
        r['copied_from'] = old if old != new else None
        r['lines'] = lines = []

        if diff.changes:
            for old, new, line in diff.changes:
                if old is None:
                    lines.append(
                        OrderedDict([('line', new), ('deleted', False), ('data', line)])
                    )
                if new is None:
                    lines.append(
                        OrderedDict([('line', old), ('deleted', True), ('data', line)])
                    )

        res.append(r)
    res = {'diffs': res}

    f = os.path.splitext(f)[0]

    with open(os.path.join('output', f + '.json'), 'w') as Out:
        json.dump(res, Out, indent=4, separators=(',', ': '))
