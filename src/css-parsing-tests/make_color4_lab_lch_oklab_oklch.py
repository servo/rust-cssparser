items = []


def lab(lightness, a, b, alpha=1):
    return (lightness, a, b, alpha)


def labp(lightness, a, b, alpha=1):
    return (lightness, a * 125.0 / 100.0, b * 125.0 / 100.0, alpha)


def oklab(lightness, a, b, alpha=1):
    return (lightness, a, b, alpha)


def oklabp(lightness, a, b, alpha=1):
    return (lightness / 100.0, a * 0.4 / 100.0, b * 0.4 / 100.0, alpha)


def slab(name, lightness, a, b, alpha=1):
    if alpha == 1.0:
        return '{}({:g} {:g} {:g})'.format(name, lightness, a, b)
    else:
        return '{}({:g} {:g} {:g} / {:g})'.format(name, lightness, a, b, alpha)


def lab_like(name, f, fp):
    for b in [0.0, 10.0, 110.0, -10.0]:
        for a in [0.0, 10.0, 110.0, -10.0]:
            for lightness in [0.0, 10.0, 110.0, -10.0]:
                items.append('"{}({:g} {:g} {:g})", "{:s}"'.format(
                    name, lightness, a, b, slab(name, *f(lightness, a, b))))
                items.append('"{}({:g}% {:g}% {:g}%)", "{:s}"'.format(
                    name, lightness, a, b, slab(name, *fp(lightness, a, b))))
                for alpha in [0, 0.2, 1]:
                    items.append('"{}({:g} {:g} {:g} / {:g})", "{:s}"'.format(
                        name, lightness, a, b, alpha, slab(name, *f(lightness, a, b, alpha))))
                    items.append('"{}({:g}% {:g}% {:g}% / {:g})", "{:s}"'.format(
                        name, lightness, a, b, alpha, slab(name, *fp(lightness, a, b, alpha))))


lab_like('lab', lab, labp)
lab_like('oklab', oklab, oklabp)


def calc_deg(deg):
    while deg >= 360.0:
        deg -= 360.0
    while deg < 0.0:
        deg += 360.0

    return deg


def lch(lightness, c, h, alpha=1):
    return (lightness, c, calc_deg(h), alpha)


def lchp(lightness, c, h, alpha=1):
    return (lightness, c * 150.0 / 100.0, calc_deg(h), alpha)


def oklch(lightness, c, h, alpha=1):
    return (lightness, c, calc_deg(h), alpha)


def oklchp(lightness, c, h, alpha=1):
    return (lightness / 100.0, c * 0.4 / 100.0, calc_deg(h), alpha)


def slch(name, lightness, c, h, alpha=1):
    if alpha == 1:
        return '{}({:g} {:g} {:g})'.format(name, lightness, c, h)
    else:
        return '{}({:g} {:g} {:g} / {:g})'.format(name, lightness, c, h, alpha)


def lch_like(name, f, fp):
    for h in [0, 30, 60, 90, 120, 180, 210, 240, 270, 300, 330, 360, 380, 700, -20]:
        for c in [0.0, 10.0, 110.0, -10.0]:
            for lightness in [0.0, 10.0, 110.0, -10.0]:
                items.append('"{}({:g} {:g} {:g})", "{:s}"'.format(
                    name, lightness, c, h, slch(name, *f(lightness, c, h))))
                items.append('"{}({:g}% {:g}% {:g}deg)", "{:s}"'.format(
                    name, lightness, c, h, slch(name, *fp(lightness, c, h))))
                for alpha in [0, 0.2, 1]:
                    items.append('"{}({:g} {:g} {:g} / {:g})", "{:s}"'.format(
                        name, lightness, c, h, alpha, slch(name, *f(lightness, c, h, alpha))))
                    items.append('"{}({:g}% {:g}% {:g}deg / {:g})", "{:s}"'.format(
                        name, lightness, c, h, alpha, slch(name, *fp(lightness, c, h, alpha))))


lch_like('lch', lch, lchp)
lch_like('oklch', oklch, oklchp)


print('[')
print(',\n'.join(items))
print(']')
