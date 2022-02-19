import os
import sys


def main() -> int:
    if not os.path.exists('output'):
        os.mkdir('output')
    if not os.path.exists('output/TEData'):
        os.mkdir('output/TEData')

    return 0


if __name__ == '__main__':
    sys.exit(main())
