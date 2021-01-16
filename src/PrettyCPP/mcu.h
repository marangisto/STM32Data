#pragma once

////
//
//      {{family}} MCU family
//
///

enum mcu_t
{{#mcus}}
    {{#first}}{ {{/first}}{{^first}}, {{/first}}{{refName}}
{{/mcus}}
    };

enum svd_t
{{#svds}}
    {{#first}}{ {{/first}}{{^first}}, {{/first}}{{name}} = {{value}}
{{/svds}}
    };

enum gpio_conf_t
{{#configs}}
    {{#first}}{ {{/first}}{{^first}}, {{/first}}{{name}} = {{value}}
{{/configs}}
    };

enum dma_resource_t
{{#dmaResource}}
    {{#first}}{ {{/first}}{{^first}}, {{/first}}{{name}}
{{/dmaResource}}
{{^dmaResource}}
    {
{{/dmaResource}}
    };

enum periph_t
{{#periphs}}
    {{#first}}{ {{/first}}{{^first}}, {{/first}}{{name}}
{{/periphs}}
    };

template<svd_t SVD, periph_t PERIPH>
struct peripheral_t
{
    static_assert
        ( always_false_i<SVD>::value
        , "peripheral not available on this MCU!"
        );
};

template<mcu_t MCU> struct mcu_traits {};

{{#mcus}}
template<> struct mcu_traits<{{refName}}>
{
    static constexpr family_t family = {{familyEnum}};
    static constexpr svd_t svd = {{svd}};
    static constexpr gpio_conf_t gpio_conf = {{gpioConf}};
    static constexpr unsigned frequency = {{frequency}};
};

{{/mcus}}
// Set target from command-line -DMCU= option
static constexpr mcu_t target = MCU;

