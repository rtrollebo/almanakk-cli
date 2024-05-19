tags=$(git tag | sort -r -n)
version=$(echo $tags | head -1)
m4 -D VERSION=$version version.m4 > src/Almanakk/Application/Version.hs