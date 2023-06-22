pkgname=prefics-scheme-git
pkgver=0.1.0
pkgrel=1
pkgdesc="A scheme language implementation"
arch=('x86_64')
url="https://github.com/prefics/scheme"
license=('MIT')
makedepends=()
source=("$pkgname::git+http://github.com/prefics/scheme.git")
sha256sums=('SKIP')

build() {
    cd "$pkgname"
    ./configure --prefix=/usr
    make
}

package() {
    cd "$pkgname"
    make DESTDIR="$pkgdir/" install
}
