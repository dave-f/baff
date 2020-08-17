# baff
Create a header containing a byte array generated from a file.

Sometimes it is desirable to generate a byte array from a file for directly including into source code.  `baff` provides this functionality and is invoked thus: 

```
M-x RET baff RET filename RET
```

This will read the bytes from `filename` and create a buffer similar to:

````
#include <array>

// source : c:/dev/baff/README.md
// sha256 : b2d9aa7ed942cf08d091f3cce3dd56603fef00f5082573cd8b8ad987d29d4351

std::array<uint8_t,10> bytes = {
    0x23, 0x20, 0x62, 0x61, 0x66, 0x66, 0x0d, 0x0a, 0x43, 0x72 
};
````

The default style is C++ but this can be changed to whatever style is desired via `M-x customize-group RET baff RET`
