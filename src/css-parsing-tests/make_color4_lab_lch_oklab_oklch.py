items = []


def cielab(lightness, a, b, alpha=1, clamp=False):
    if clamp:
        lightness = max(0.0, min(lightness, 100.0))

    return (lightness, a * 125.0 / 100.0, b * 125.0 / 100.0, alpha)


def oklab(lightness, a, b, alpha=1, clamp=False):
    if clamp:
        lightness = max(0.0, min(lightness, 100.0))

    return (lightness / 100.0, a * 0.4 / 100.0, b * 0.4 / 100.0, alpha)


def slab(name, lightness, a, b, alpha=1):
    if alpha == 1.0:
        return '{}({:g} {:g} {:g})'.format(name, lightness, a, b)
    else:
        return '{}({:g} {:g} {:g} / {:g})'.format(name, lightness, a, b, alpha)


def lab_like(name, lab):
    percentages = [ 0.0, 10.0, 25.0, 33.33, 50.0, 66.67, 75.0, 90.0, 100.0, -10.0, 110.0 ]

    for b in percentages:
        for a in percentages:
            for lightness in percentages:
                items.append('"{}({:g}% {:g}% {:g}%)", [[{:#g}, {:#g}, {:#g}, {:#g}], "{:s}"]'.format(
                    name, lightness, a, b, *lab(lightness, a, b), slab(name, *lab(lightness, a, b, clamp=True))))

                items.append('"{}({:g} {:g} {:g})", [[{:#g}, {:#g}, {:#g}, {:#g}], "{:s}"]'.format(
                    name, *lab(lightness, a, b)[:3], *lab(lightness, a, b), slab(name, *lab(lightness, a, b, clamp=True))))

                for alpha in [0, 0.2, 1]:
                    items.append('"{}({:g}% {:g}% {:g}% / {:g})", [[{:#g}, {:#g}, {:#g}, {:#g}], "{:s}"]'.format(
                        name, lightness, a, b, alpha, *lab(lightness, a, b, alpha), slab(name, *lab(lightness, a, b, alpha, clamp=True))))

                    items.append('"{}({:g} {:g} {:g} / {:g})", [[{:#g}, {:#g}, {:#g}, {:#g}], "{:s}"]'.format(
                        name, *lab(lightness, a, b, alpha), *lab(lightness, a, b, alpha), slab(name, *lab(lightness, a, b, alpha, clamp=True))))


lab_like('lab', cielab)
lab_like('oklab', oklab)


def cielch(lightness, c, h, alpha=1, clamp=False):
    if clamp:
        lightness = max(0.0, min(lightness, 100.0))
        c = max(0.0, c)

    return (lightness, c * 150.0 / 100.0, h, alpha)


def oklch(lightness, c, h, alpha=1, clamp=False):
    if clamp:
        lightness = max(0.0, min(lightness, 100.0))
        c = max(0.0, c)

    return (lightness / 100.0, c * 0.4 / 100.0, h, alpha)


def slch(name, lightness, c, h, alpha=1):
    if alpha == 1:
        return '{}({:g} {:g} {:g})'.format(name, lightness, c, h)
    else:
        return '{}({:g} {:g} {:g} / {:g})'.format(name, lightness, c, h, alpha)


def lch_like(name, lch):
    percentages = [ 0.0, 10.0, 25.0, 33.33, 50.0, 66.67, 75.0, 90.0, 100.0, -10.0, 110.0 ]

    for h in [0, 30, 60, 90, 120, 180, 210, 240, 270, 300, 330, 360, 380, 700, -20]:
        for c in percentages:
            for lightness in percentages:
                items.append('"{}({:g}% {:g}% {:g}deg)", [[{:#g}, {:#g}, {:#g}, {:#g}], "{:s}"]'.format(
                    name, lightness, c, h, *lch(lightness, c, h), slch(name, *lch(lightness, c, h, clamp=True))))

                items.append('"{}({:g} {:g} {:g})", [[{:#g}, {:#g}, {:#g}, {:#g}], "{:s}"]'.format(
                    name, *lch(lightness, c, h)[:3], *lch(lightness, c, h), slch(name, *lch(lightness, c, h, clamp=True))))

                for alpha in [0, 0.2, 1]:
                    items.append('"{}({:g}% {:g}% {:g}deg / {:g})", [[{:#g}, {:#g}, {:#g}, {:#g}], "{:s}"]'.format(
                        name, lightness, c, h, alpha, *lch(lightness, c, h, alpha), slch(name, *lch(lightness, c, h, alpha, clamp=True))))

                    items.append('"{}({:g} {:g} {:g} / {:g})", [[{:#g}, {:#g}, {:#g}, {:#g}], "{:s}"]'.format(
                        name, *lch(lightness, c, h, alpha), *lch(lightness, c, h, alpha), slch(name, *lch(lightness, c, h, alpha, clamp=True))))


lch_like('lch', cielch)
lch_like('oklch', oklch)


print('[')
print(',\n'.join(items))
print(']')
