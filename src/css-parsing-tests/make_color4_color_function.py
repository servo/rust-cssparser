COLOR_SPACES = [
    'srgb',
    'srgb-linear',
    'display-p3',
    'a98-rgb',
    'prophoto-rgb',
    'rec2020',
    'xyz',
    'xyz-d50',
    'xyz-d65'
]
PERMUTATIONS = [
    ('0% 0% 0%', '0 0 0'),
    ('10% 10% 10%', '0.1 0.1 0.1'),
    ('.2 .2 25%', '0.2 0.2 0.25'),
    ('0 0 0 / 1', '0 0 0'),
    ('0% 0 0 / 0.5', '0 0 0 / 0.5'),
    ('20% 0 10/0.5', '0.2 0 10 / 0.5'),
    ('20% 0 10/50%', '0.2 0 10 / 0.5'),
    ('400% 0 10/50%', '4 0 10 / 0.5'),
    ('50% -160 160', '0.5 -160 160'),
    ('50% -200 200', '0.5 -200 200'),
    ('0 0 0 / -10%', '0 0 0 / 0'),
    ('0 0 0 / 110%', '0 0 0'),
    ('0 0 0 / 300%', '0 0 0'),
    ('200 200 200', '200 200 200'),
    ('200 200 200 / 200', '200 200 200'),
    ('-200 -200 -200', '-200 -200 -200'),
    ('-200 -200 -200 / -200', '-200 -200 -200 / 0'),
    ('200% 200% 200%', '2 2 2'),
    ('200% 200% 200% / 200%', '2 2 2'),
    ('-200% -200% -200% / -200%', '-2 -2 -2 / 0'),
]


def result_color_space(color_space):
    if color_space == 'xyz':
        return 'xyz-d65'
    else:
        return color_space


lines = []
for color_space in COLOR_SPACES:
    for permutation in PERMUTATIONS:
        lines.append('  "color({} {})", "color({} {})"'.format(
            color_space, permutation[0], result_color_space(color_space), permutation[1]))


print("[")
print(',\n'.join(lines))
print("]")
