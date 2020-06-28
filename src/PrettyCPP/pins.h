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

enum altfun_t
{{#altfuns}}
    {{#first}}{ {{/first}}{{^first}}, {{/first}}{{name}} = {{value}}
{{/altfuns}}
    };

enum signal_t
{{#signals}}
    {{#first}}{ {{/first}}{{^first}}, {{/first}}{{name}}
{{/signals}}
    };

static constexpr gpio_conf_t gpio_conf = mcu_traits<target>::gpio_conf;

template<gpio_conf_t CFG, pin_t PIN, signal_t SIG>
struct signal_traits
{
    static constexpr altfun_t AF = AF0;
    static_assert
        ( always_false_i<PIN>::value
        , "alternate function not available on this pin!"
        );
};

template<bool AVAIL>
struct available_signal_t
{
    using type = altfun_t;
    static assert
        ( always_false_i<AVAIL>::value
        , "alternate function not available on pin for mcu"
        );
};

template<>
struct available_signal_t<true>
{
    using type = altfun_t;
};

template<bool AVAIL>
using altfun = typename available_signal_t<AVAIL>::type;
{{#traits}}

template<gpio_conf_t CFG>
struct signal_traits<CFG, {{pin}}, {{signal}}>
{
{{^condLHS}}
    static constexpr altfun_t AF = {{#simpleRHS}}{{altfun}}{{/simpleRHS}};
{{/condLHS}}
{{#condLHS}}
    static constexpr altfun<CFG&{{config}}> AF = {{#simpleRHS}}{{altfun}}{{/simpleRHS}};
{{/condLHS}}
};
{{/traits}}
{{/gpio}}

