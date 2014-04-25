{
    "variables" : [
        {
            "id": "a",
            "type": "cycle",
            "element_type": "uint8",
            "start": 0,
            "end": 20,
            "spacing": 3
        }
    ],
    "message": {
        "mid": "0xCA",
        "cc" : 11,
        "arguments": [
            "a",
            {
                "type": "uint8",
                "label": "b",
                "value": 2
            },
            {
                "type": "uint8",
                "label": "c",
                "value": 3
            },
            {
                "type": "uint8",
                "label": "d",
                "value": 4
            },
            {
                "type": "string",
                "length": 8,
                "label": "str",
                "value": "abcd"
            },
            {
                "type": "array",
                "element_type": "uint8",
                "label": "intArr",
                "values": [5, 6, 7, 8]
            },
            {
                "type": "float",
                "label": "e",
                "value": 9
            },
            {
                "type": "double",
                "label": "f",
                "value": 10
            },
            {
                "type": "uint32",
                "label": "g",
                "value": 11
            },
            {
                "type": "uint64",
                "label": "h",
                "value": 12
            }
        ]
    }
}