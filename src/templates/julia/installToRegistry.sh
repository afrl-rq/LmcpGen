# Example commands to install locally. Must be done if not activated.
cd LMCP
git init 
git add .
git commit -m "init"
julia -e 'using Pkg; Pkg.add(path=pwd())'