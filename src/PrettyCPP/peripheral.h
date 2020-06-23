#pragma once

////
//
//      {{family}} {{group}} peripherals
//
///
{{#periphTypes}}

struct {{#typeRef}}{{svdLC}}_{{nameLC}}_t{{/typeRef}}
{
{{#registers}}
    volatile uint32_t {{name}}; // {{description}}
{{/registers}}
};
{{/periphTypes}}

