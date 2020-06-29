#pragma once

////
//
//      {{family}} vectors
//
///
{{#interrupt}}

{{#interrupts}}
{{^first}}
{{^pad}}
template<> void handler<interrupt::{{name}}>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
{{/pad}}
{{/first}}
{{/interrupts}}

void (*vectors[])(void) __attribute__ ((section(".vectors"))) =
    { (void(*)(void)) &__estack // -16: Initial stack pointer
{{#interrupts}}
{{#pad}}
    , 0x0
{{/pad}}
{{^pad}}
    , handler<interrupt::{{name}}> // {{value}}: {{description}}
{{/pad}}
{{/interrupts}}
    };
{{/interrupt}}

