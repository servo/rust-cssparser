items = []


def lab(lightness, a, b, alpha=1):
    return (max(0.0, lightness), a, b, round(alpha * 100000) * 255 / 100000)


def labp(lightness, a, b, alpha=1):
    return (max(0.0, lightness), a * 125.0 / 100.0, b * 125.0 / 100.0, round(alpha * 100000) * 255 / 100000)


def oklab(lightness, a, b, alpha=1):
    return (max(0.0, lightness), a, b, round(alpha * 100000) * 255 / 100000)


def oklabp(lightness, a, b, alpha=1):
    return (max(0.0, lightness / 100.0), a * 0.4 / 100.0, b * 0.4 / 100.0, round(alpha * 100000) * 255 / 100000)


def slab(name, lightness, a, b, alpha=255):
    if alpha == 255:
        return '{}({:g} {:g} {:g})'.format(name, lightness, a, b)
    else:
        return '{}({:g} {:g} {:g} / {:g})'.format(name, lightness, a, b, alpha / 255.0)


def lab_like(name, f, fp):
    for b in [0.0, 10.0, 110.0, -10.0]:
        for a in [0.0, 10.0, 110.0, -10.0]:
            for lightness in [0.0, 10.0, 110.0, -10.0]:
                items.append('"{}({:g} {:g} {:g})", [[{:g}, {:g}, {:g}, {:g}], "{:s}"]'.format(
                    name, lightness, a, b, *f(lightness, a, b), slab(name, *f(lightness, a, b))))
                items.append('"{}({:g}% {:g}% {:g}%)", [[{:g}, {:g}, {:g}, {:g}], "{:s}"]'.format(
                    name, lightness, a, b, *fp(lightness, a, b), slab(name, *fp(lightness, a, b))))
                for alpha in [0, 0.2, 1]:
                    items.append('"{}({:g} {:g} {:g} / {:g})", [[{:g}, {:g}, {:g}, {:g}], "{:s}"]'.format(
                        name, lightness, a, b, alpha, *f(lightness, a, b, alpha), slab(name, *f(lightness, a, b, alpha))))
                    items.append('"{}({:g}% {:g}% {:g}% / {:g})", [[{:g}, {:g}, {:g}, {:g}], "{:s}"]'.format(
                        name, lightness, a, b, alpha, *fp(lightness, a, b, alpha), slab(name, *fp(lightness, a, b, alpha))))


lab_like('lab', lab, labp)
lab_like('oklab', oklab, oklabp)


def calc_deg(deg):
    while deg >= 360.0:
        deg -= 360.0
    while deg < 0.0:
        deg += 360.0

    return deg


def lch(lightness, c, h, alpha=1):
    return (max(lightness, 0.0), max(0.0, c), calc_deg(h), round(alpha * 100000) * 255 / 100000)


def lchp(lightness, c, h, alpha=1):
    return (max(lightness, 0.0), max(0.0, c * 150.0 / 100.0), calc_deg(h), round(alpha * 100000) * 255 / 100000)


def oklch(lightness, c, h, alpha=1):
    return (max(lightness, 0.0), max(0.0, c), calc_deg(h), round(alpha * 100000) * 255 / 100000)


def oklchp(lightness, c, h, alpha=1):
    return (max(lightness, 0.0) / 100.0, max(0.0, c * 0.4 / 100.0), calc_deg(h), round(alpha * 100000) * 255 / 100000)


def slch(name, lightness, c, h, alpha=1):
    if alpha == 255:
        return '{}({:g} {:g} {:g})'.format(name, lightness, c, h)
    else:
        return '{}({:g} {:g} {:g} / {:g})'.format(name, lightness, c, h, alpha / 255.0)


def lch_like(name, f, fp):
    for h in [0, 30, 60, 90, 120, 180, 210, 240, 270, 300, 330, 360, 380, 700, -20]:
        for c in [0.0, 10.0, 110.0, -10.0]:
            for lightness in [0.0, 10.0, 110.0, -10.0]:
                items.append('"{}({:g} {:g} {:g})", [[{:g}, {:g}, {:g}, {:g}], "{:s}"]'.format(
                    name, lightness, c, h, *f(lightness, c, h), slch(name, *f(lightness, c, h))))
                items.append('"{}({:g}% {:g}% {:g}deg)", [[{:g}, {:g}, {:g}, {:g}], "{:s}"]'.format(
                    name, lightness, c, h, *fp(lightness, c, h), slch(name, *fp(lightness, c, h))))
                for alpha in [0, 0.2, 1]:
                    items.append('"{}({:g} {:g} {:g} / {:g})", [[{:g}, {:g}, {:g}, {:g}], "{:s}"]'.format(
                        name, lightness, c, h, alpha, *f(lightness, c, h, alpha), slch(name, *f(lightness, c, h, alpha))))
                    items.append('"{}({:g}% {:g}% {:g}deg / {:g})", [[{:g}, {:g}, {:g}, {:g}], "{:s}"]'.format(
                        name, lightness, c, h, alpha, *fp(lightness, c, h, alpha), slch(name, *fp(lightness, c, h, alpha))))


lch_like('lch', lch, lchp)
lch_like('oklch', oklch, oklchp)


print('[')
print(',\n'.join(items))
print(']')
