import codecs
from collections import OrderedDict
import json
import os
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


directory = './patches'
for f in os.listdir(directory):
    with open(os.path.join(directory, f), 'rb') as In:
        patch = In.read()

    patch = patch.decode('utf-8', errors='myreplacement')

    res = []
    for diff in whatthepatch.parse_patch(patch):
        r = OrderedDict()

        old = diff.header.old_path
        new = diff.header.new_path

        old = old[2:] if old.startswith('a/') else old
        new = new[2:] if new.startswith('b/') else new

        r['filename'] = new
        r['new'] = False
        r['deleted'] = False
        r['binary'] = False
        r['renamed_from'] = old if old != new else None
        r['lines'] = lines = []

        if f == 'b184c87f7606.patch':
            from pprint import pprint

            # pprint(diff.changes)

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

    f = os.path.splitext(f)[0]

    with open(os.path.join('output', f + '.json'), 'w') as Out:
        json.dump(res, Out, indent=4, separators=(',', ': '))
