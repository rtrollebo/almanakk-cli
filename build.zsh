if [[ "$1" == "mock" ]]; then
    stack build 
else
    stack build --stack-yaml stack-full.yaml --flag almanakk-cli:use-external-lib   
fi
