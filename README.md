Auto
====

Command and Telemetry Sequencer for NASA's Core Flight Executive

http://code.nasa.gov/project/core-flight-executive-cfe/


Code temporarily [here](https://cusat.cornell.edu/viewvc/ITAR/Violet/FlightSoftware/branches/ACS/cfe/tools/Auto).

[JSON Editor](http://www.alkemis.com/jsonEditor.htm)


## Usage

A test is defined by a set of message files (`*.tlm` and `*.cmd`) and a single control file (`*.ctrl`), both using the [JSON](http://www.json.org/) format. Together these define the information in the command and telemetry packets, when to send them, where to send them, and how they should change over time.

To run a test:

1. Create / edit the control and message files (see below)
2. `./auto <control file>`

### Control File Specification

The purpose of a control file is to define which, how, and when messages are sent. An example control file is in `$CFS_MISSION/cfe/tools/Auto/all.ctrl`.

Control files have three required parts:

1. `meta`
    - `ip: <string>` -- the destination ip address
    - `port: <int>` -- the destination port (use 1234 for `CI_lab`)
    - `endianness: <string>` -- LE or BE (not implemented yet, only LE currently)
2. `sequenced: <list>` -- a list of **message metadata objects**. These messages will be sent in series (sequentially).
3. `parallel: <list>` -- a list of **message metadata objects**. These will be sent concurrently (in parallel)

where:

- **message metadata object**
    - `file: <string>` -- the message file name
    - `frequency: <double>` -- the frequency to send the message (Hz)
    - `times: <int>` -- the number of times to repeat the message


### Message File Specification

A message file defines the data in a command (`*.cmd`) or telemetry (`*.tlm`) message, and how it changes over time. Several examples of each can be found in `$CFS_MISSION/cfe/tools/Auto/`. The basic idea is to use the `message` section to define the data in the header and payload. To give an argument (command) or parameter (telemetry) in the payload a changing value, insert a variable name in its place. Then, define how that variable should change in the `variables` section.

The message definition has two required parts:

1. `variables`:
    - `<list>` -- a list of **variable definition objects**
2. `message`:
    - `mid: <string>`: A string representation of the message id (e.g. "127" or "0x7F")
    - for command files
        - `cc: <int>` -- the command code
        - `arguments: <list>` -- a list of **data definition objects**
    - for telemetry files
        - `parameters: <list>` -- a list of **data definition objects**

where:

- **variable definition object**
    - `id: <string>`  -- the name (identifier) for the variable
    - `element_type: <string>` -- the type of the argument/parameter being varied
    - `type: <string>` -- one of "random", "cycle", or "sequence". The remaining keys of the variable definition object are dependent on this value.

    - if `type=="random":` -- (`element_type` must be numeric)
        Generates an infinite list of random values within specified bounds.
        - `low: <element_type>` -- inclusive minimum
        - `high: <element_type>` -- inclusive maximum

    - if `type=="cycle":` -- (`element_type` must be numeric)
        Generates an infinite list of values cycling between specified endpoints with specified spacing
        - `start: <element_type>` -- inclusive start value
        - `end: <element_type>` -- inclusive end value
        - `spacing: <element_type>` -- the stepsize to use in the sequence.

    - if `type=="sequence":` -- (`element_type` can be anything)
        Generates an infinite list of specified values, looping back to the start after finishing.
        - `values: <list <element_type>>`

- **data definition object**

    `<object>|<string>` -- use the object for unchanging values, and the string for variables (use the variable's id). For the object case, the keys are:

    `label: <string>` -- name for the data, only used when printing packet (not sent in packet)
    `type: <string>` -- one of "bool" | "float" | "double" | "uint8" | "uint16" | "uint32" | "uint64" | "int8" | "int16" | "int32"| "int64" | "string" | "array". The remaining keys in the data definition object are dependent on this value.
    - `if type="string":`
        - `length: <int>` -- the size of the string buffer
        - `value: <string>` -- the string itself
    - if `type="array":`
        - `element_type: <string>`
        - `values: <list <type>>` -- a list of values. arrays containing strings, variables, or other arrays are not supported. Variable values inside arrays can be achieved by breaking the parameter up into three definitions: the portion of the array before the variable, the variable itself, and the portion of the array after the variable.
    - if `type=<other_type>:`
        - `value: <other_type>` -- Wrap strings in quotes. Use `true` and `false` for bools.
