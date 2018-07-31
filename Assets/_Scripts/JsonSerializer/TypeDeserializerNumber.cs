using UnityEngine;

using System;
using System.Collections;
using System.Collections.Generic;

namespace Nanome.Maths.Serializers.JsonSerializer
{

    public class TypeDeserializerNumber : TypeDeserializer
    {

        Dictionary<char, double> digits = new Dictionary<char, double>();
        HashSet<char> specials = new HashSet<char>();

        public TypeDeserializerNumber()
        {
            digits['0'] = 0;
            digits['1'] = 1;
            digits['2'] = 2;
            digits['3'] = 3;
            digits['4'] = 4;
            digits['5'] = 5;
            digits['6'] = 6;
            digits['7'] = 7;
            digits['8'] = 8;
            digits['9'] = 9;
            specials.Add('-');
            specials.Add('+');
            specials.Add('.');
        }

        public override object Deserialize(ContextDeserialization context)
        {
            // Parsing flags
            var isDecimal = false;
            var bigValue = (double)0;
            var decimalPoint = (double)1;
            var multiplier = 1;
            // While we have numbers chars
            while (true)
            {
                // Keep reading
                var curr = context.ReadChar();
                // If it is a digit
                var digit = (double)0;
                if (digits.TryGetValue(curr, out digit))
                {
                    if (isDecimal)
                    {
                        decimalPoint *= 10;
                    }
                    bigValue *= 10;
                    bigValue += digit;
                }
                // If its something else
                else
                {
                    // If special char, set some flags
                    if (specials.Contains(curr))
                    {
                        if (curr == '.')
                        {
                            isDecimal = true;
                        }
                        if (curr == '-')
                        {
                            multiplier *= -1;
                        }
                    }
                    // If no known, it means we just hit the end of the number
                    else
                    {
                        break;
                    }
                }
                // Burn parsed digit
                context.Burn(1);
            }
            // If result is decimal
            if (isDecimal)
            {
                return (double)(multiplier * (bigValue / decimalPoint));
            }
            // If result is integer
            else
            {
                return (long)(multiplier * bigValue);
            }
        }

    }

}
