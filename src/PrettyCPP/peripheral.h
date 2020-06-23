#pragma once

////
//
//      {{family}} {{group}} peripherals
//
///
{{#periphTypes}}

struct {{#typeRef}}{{svd}}_{{name}}_t{{/typeRef}}
{
};
{{/periphTypes}}

