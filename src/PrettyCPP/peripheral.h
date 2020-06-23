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
  {{#reserve}}
    reserved_t<{{size}}> {{name}};
  {{/reserve}}
  {{^reserve}}
    {{type}} {{name}}; // {{description}}
  {{/reserve}}
{{/registers}}
};
{{/periphTypes}}

