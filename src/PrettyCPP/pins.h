#pragma once

////
//
//      {{family}} pins
//
///
{{#gpio}}

enum port_t
{{#ports}}
    {{#first}}{ {{/first}}{{^first}}, {{/first}}{{name}} = {{value}}
{{/ports}}
    };

enum pin_t
{{#pins}}
    {{#first}}{ {{/first}}{{^first}}, {{/first}}{{name}} = {{value}}
{{/pins}}
    };

enum af_t
{{#afs}}
    {{#first}}{ {{/first}}{{^first}}, {{/first}}{{name}} = {{value}}
{{/afs}}
    };

enum alt_fun_t
{{#altFuns}}
    {{#first}}{ {{/first}}{{^first}}, {{/first}}{{name}}
{{/altFuns}}
    };
{{/gpio}}

static constexpr gpio_conf_t gpio_conf = mcu_traits<target>::gpio_conf;

template<gpio_conf_t CFG, pin_t PIN, alt_fun_t ALT>
struct alt_fun_traits
{
    static constexpr af_t AF = AF0;
    static_assert
        ( always_false_i<PIN>::value
        , "alternate function not available on this pin!"
        );
};

template<bool AVAIL>
struct available_alt_fun_t
{
    using type = alt_fun_t;
    static assert
        ( always_false_i<AVAIL>::value
        , "alternate function not available on pin for mcu"
        );
};

template<>
struct available_alt_fun_t<true>
{
    using type = alt_fun_t;
};

