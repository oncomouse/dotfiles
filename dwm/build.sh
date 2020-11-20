patches=(
  "pango/dwm-pango-20201020-519f869.diff",
  "ewmhtags/dwm-ewmhtags-6.2.diff"
)

git clone git://git.suckless.org/dwm
cd dwm || exit
for patch in "${patches[@]}"; do
  curl -s "https://dwm.suckless.org/patches/$patch" | patch -p1 -l
done
cp ../config.h .
cp ../config.mk .
make
sudo make install
cd ..
rm -rf dwm
