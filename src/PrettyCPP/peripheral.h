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
struct {{#instRef}}peripheral_t<{{svd}}, {{name}}>{{/instRef}}
{
    static constexpr periph_t P = {{#instRef}}{{name}}{{/instRef}};
    using T = {{#typeRef}}{{svdLC}}_{{nameLC}}{{/typeRef}}_t;
    static T& V;
};
{{/periphInsts}}
{{/periphTypes}}

{{#peripherals}}
using {{nameLC}}_t = peripheral_t<svd, {{name}}>;
{{/peripherals}}
{{#needTraits}}

template<int INST> struct {{groupLC}}_traits {};
{{/needTraits}}
{{#peripherals}}
{{#haveTraits}}
{{#instNo}}

template<> struct {{groupLC}}_traits<{{instNo}}>
{
    using {{groupLC}} = {{nameLC}}_t;
{{#altFuns}}
    static constexpr signal_t {{altFun}} = {{name}}_{{altFun}};
{{/altFuns}}
{{#controls}}

    template<typename RCC>
    static void {{method}}()
    {
  {{#en}}
        RCC::V.{{register}} |= RCC::T::{{register}}_{{flag}};
  {{/en}}
  {{^en}}
        RCC::V.{{register}} &= ~RCC::T::{{register}}_{{flag}};
  {{/en}}
    }
{{/controls}}
};
{{/instNo}}
{{/haveTraits}}
{{/peripherals}}
