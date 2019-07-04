# HaskMAVLink

This is a library to generate haskell modules for the [mavlink](https://mavlink.io/en/messages/common.html) protocol. 

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

For help with the mavgen tool:
```
stack exec mavgen-exe -- --help
```

### Running the test

This is a sample application that uses the generated mavlink modules to talk to simple python mavlink server [test/pymavlink_server.py](test/pymavlink_server.py). 

- First generate the haskell modules for the [common.xml](test/common.xml) mavlink definition. 

```
stack exec mavgen-exe -- -i test/common.xml -o test
```

- Launch the python server to send and receive mavlink packets to the test application

```
python3 test/pymavlink_server.py
```

- Run the sample test application
```
stack test
```

See [test/Spec.hs](test/Spec.hs) for example on using the generated haskell modules


