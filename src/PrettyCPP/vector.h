#pragma once

////
//
//      {{family}} vectors
//
///
{{#interrupt}}

{{#interrupts}}
{{^first}}
template<> void handler<interrupt::{{name}}>() __attribute__ ((weak, alias("_Z17__default_handlerv")));
{{/first}}
{{/interrupts}}

void (*vectors[])(void) __attribute__ ((section(".vectors"))) =
    { (void(*)(void)) &__estack // -16: Initial stack pointer
{{#interrupts}}
    , handler<interrupt::{{name}}> // {{value}}: {{description}}
{{/interrupts}}
    };
{{/interrupt}}

