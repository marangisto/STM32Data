#pragma once

////
//
//      {{family}} peripheral instances
//
///
{{#periphInsts}}
{{#instRef}}

typename peripheral_t<{{svd}}, {{name}}>::T&
    peripheral_t<{{svd}}, {{name}}>::V = *reinterpret_cast
        <typename peripheral_t<{{svd}}, {{name}}>::T*
        >({{baseAddress}});
{{/instRef}}
{{/periphInsts}}

