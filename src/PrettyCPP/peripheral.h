#pragma once

////
//
//      {{family}} {{group}} peripherals
//
///
{{#periphTypes}}

struct {{#typeRef}}{{svd}}_{{name}}_t{{/typeRef}}
{
{{#registers}}
    volatile uint32_t {{name}}; // {{description}}
{{/registers}}
};
{{/periphTypes}}

