#pragma once

////
//
//      {{family}} {{group}} peripherals
//
///
{{#periphTypes}}

// {{#typeRef}}{{name}}{{/typeRef}}: {{description}}

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
{{#periphTypes}}
{{#periphInsts}}

template<>
struct {{#instRef}}periptheral_t<{{svd}}, {{name}}>{{/instRef}}
{
    using T = {{#typeRef}}{{svdLC}}_{{nameLC}}{{/typeRef}};
    static T& V;
};
{{/periphInsts}}
{{/periphTypes}}

{{#abstractInsts}}
using {{nameLC}}_t = peripheral_t<svd, {{name}}>;
{{/abstractInsts}}

template<int INST> struct {{groupLC}}_traits {};
{{#abstractInsts}}

template<> struct {{groupLC}}_traits<{{instNo}}>
{
        using {{groupLC}} = {{nameLC}}_t;
};
{{/abstractInsts}}
