# HaskMAVLink

This is a library to generate haskell modules for the [mavlink](https://mavlink.io/en/messages/common.html) protocol. 

THIS IS STILL A WORK IN PROGRESS

## Usage

### Using stack
Build project using [stack](https://docs.haskellstack.org/en/stable/README):

```
stack build
```

Generate the haskell modules for a given mavlink xml definition:
``` 
stack exec mavgen-exe -- -i <path/to/xml/definition> -o <output/path/for/modules>
```


