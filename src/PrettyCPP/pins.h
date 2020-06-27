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

