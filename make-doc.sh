#! /bin/sh

echo '# sad' > README.md
echo >> README.md
echo '```' >> README.md
sad -h >> README.md
echo '```' >> README.md
