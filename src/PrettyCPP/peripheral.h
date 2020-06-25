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
{{#registers}}

 {{#fields}}
  {{#value}}
    {{type}} {{name}} = {{value}}; // {{description}}
  {{/value}}
  {{^value}}
    typedef bit_field_t<{{pos}}, {{mask}}> {{name}}; // {{description}}
  {{/value}}
 {{/fields}}
{{/registers}}
};
{{/periphTypes}}

