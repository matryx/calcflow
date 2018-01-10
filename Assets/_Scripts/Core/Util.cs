using System.Collections;
using System.Collections.Generic;
using System;

using UnityEngine;

namespace Nanome.Core
{
    public static class Util
    {
        public static object DefaultValue(this Type _type)
        {
            if (_type.IsValueType)
                return Activator.CreateInstance(_type);

            return null;
        }
        
    }
}
