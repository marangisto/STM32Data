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
    , NO_PIN
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
    static_assert
        ( always_false_i<PIN>::value
        , "alternate function not available on this pin!"
        );
};

template<bool AVAIL>
struct available_signal_t
{
    using type = altfun_t;
    static_assert
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
    static constexpr altfun_t AF = {{#simpleRHS}}{{altfun}}{{/simpleRHS}}{{#dualRHS}}(CFG & {{cond}}) ? {{altfun1}} : {{altfun2}}{{/dualRHS}};
{{/condLHS}}
{{#condLHS}}
    static constexpr altfun
        < CFG & {{config}}
        > AF = {{#simpleRHS}}{{altfun}}{{/simpleRHS}}{{#dualRHS}}(CFG & {{cond}}) ? {{altfun1}} : {{altfun2}}{{/dualRHS}};
{{/condLHS}}
};
{{/traits}}
{{#adcDacs}}
{{#first}}

template<periph_t PERIPH, pin_t PIN, int POLARITY>
struct adc_dac_chan{};
{{/first}}

template<>
struct adc_dac_chan<{{peripheral}}, {{pin}}, {{polarity}}>
{
    static constexpr uint8_t CHAN = {{channel}};
    static constexpr uint8_t BANK = {{bank}};
};
{{/adcDacs}}
{{/gpio}}

