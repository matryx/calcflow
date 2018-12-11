
using UnityEngine;
using System;
using System.Collections;

namespace Nanome.Core.Extension
{

    public static class ColorExtension
    {

        public static readonly Color TransparentWhite = new Color(1.0f, 1.0f, 1.0f, 0.0f);
        public static readonly Color NanomeHeaderGreen = new Color(0f, 255f / 255f, 213f / 255f, 1f);
        public static readonly Color NanomeSelectedGreen = new Color(0f, 224 / 225f, 145f / 255f, 1f);
        public static readonly Color NanomeHighlightGreen = new Color(10f / 255f, 160f / 255f, 140f / 255f, 1f);
        public static readonly Color NanomeSelectedBlack = new Color(19f / 255f, 19f / 255f, 19f / 255f, 1f);
        public static readonly Color NanomeRed = new Color(206f / 255f, 0f / 255f, 0f / 255f, 1f);
        public static readonly Color LightGray = new Color(0.98f, 0.98f, 0.98f, 1.0f);
        public static readonly Color PresenterYellow = new Color(255.0f / 255.0f, 255.0f / 255.0f, 141.0f / 255.0f, 255.0f / 255.0f);

        public static readonly Color GREENCOLOR = new Color(0f, 0.87f, 0.56f);
        public static readonly Color GREYCOLOR = new Color(0.73f, 0.73f, 0.73f);
        public static readonly Color DARKGREENCOLOR = new Color(10f / 255f, 160f / 255f, 140f / 255f);
        public static readonly Color DARKTRANSPARENTGREEN = new Color(36f / 255f, 48f / 255f, 58f / 255f, 155f / 255f);
        public static readonly Color BRIGHTRED = new Color(255.0f / 255.0f, 69.0f / 255.0f, 69.0f / 255.0f, 255.0f / 255.0f);

        public static bool Equals(this Color32 a, Color32 b)
        {
            return (
                a.r == b.r
                && a.g == b.g
                && a.b == b.b
                && a.a == b.a
            );
        }

        public static string toString(this Color aColor)
        {
            return toString(aColor, false);
        }

        public static string toString(this Color aColor, bool includeAlpha)
        {
            return toString((Color32)aColor, includeAlpha);
        }

        public static string toString(this Color32 aColor, bool includeAlpha)
        {
            string rs = Convert.ToString(aColor.r, 16).ToUpper();
            string gs = Convert.ToString(aColor.g, 16).ToUpper();
            string bs = Convert.ToString(aColor.b, 16).ToUpper();
            string a_s = Convert.ToString(aColor.a, 16).ToUpper();
            while (rs.Length < 2)
            {
                rs = "0" + rs;
            }
            while (gs.Length < 2)
            {
                gs = "0" + gs;
            }
            while (bs.Length < 2)
            {
                bs = "0" + bs;
            }
            while (a_s.Length < 2)
            {
                a_s = "0" + a_s;
            }
            if (includeAlpha)
            {
                return "#" + rs + gs + bs + a_s;
            }
            return "#" + rs + gs + bs;
        }

        public static Color toColor(this Color aValue)
        {
            return aValue;
        }

        public static float toWarmness(this Color c)
        {
            var hue = c.toHSB().x * 360f;
            var coldest = 240f;
            var diff = Mathf.Abs(hue - coldest);
            if (diff >= 180f)
            {
                hue += 360f;
                diff = Mathf.Abs(hue - coldest);
            }
            return diff / 180f;
        }

        public static Vector3 toHSB(this Color c)
        {
            float minValue = Mathf.Min(c.r, Mathf.Min(c.g, c.b));
            float maxValue = Mathf.Max(c.r, Mathf.Max(c.g, c.b));
            float delta = maxValue - minValue;
            float h = 0f;
            float s = 0f;
            float b = maxValue;
            // Calc hue (in degrees between 0 and 360)
            if (maxValue == c.r)
            {
                if (c.g >= c.b)
                {
                    if (delta == 0f)
                    {
                        h = 0f;
                    }
                    else
                    {
                        h = 60f * (c.g - c.b) / delta;
                    }
                }
                else if (c.g < c.b)
                {
                    h = 60f * (c.g - c.b) / delta + 360f;
                }
            }
            else if (maxValue == c.g)
            {
                h = 60f * (c.b - c.r) / delta + 120f;
            }
            else if (maxValue == c.b)
            {
                h = 60f * (c.r - c.g) / delta + 240f;
            }
            // Calc saturation (0 - 1)
            if (maxValue == 0f)
            {
                s = 0f;
            }
            else
            {
                s = 1f - (minValue / maxValue);
            }
            return new Vector3(h / 360f, s, b);
        }

        public static float hue(this Color c)
        {
            return toHSB(c).x;
        }

        public static float saturation(this Color c)
        {
            return toHSB(c).y;
        }

        public static float brightness(this Color c)
        {
            return toHSB(c).z;
        }

        public static float colorDistance(this Color color1, Color color2)
        {
            Vector3 v1 = new Vector3(color1.r, color1.g, color1.b);
            Vector3 v2 = new Vector3(color2.r, color2.g, color2.b);
            return Vector3.Distance(v1, v2);
        }

        public static float grayScale(this Color color)
        {
            return (color.r + color.g + color.b) / 3f;
        }

        public static Color fromHex(this Color color, string hex)
        {
            ColorUtility.TryParseHtmlString(hex, out color);
            return color;
        }

        public static Color blend(this Color color, Color other, float ratio = 0.5f)
        {
            return (1 - ratio) * color + ratio * other;
        }

        public static Color blend(this Color c1, float r1, Color c2, float r2, Color c3, float r3, Color c4, float r4)
        {
            return ((c1 * r1) + (c2 * r2) + (c3 * r3) + (c4 * r4)) / (r1 + r2 + r3 + r4);
        }

        public static Color blend(this Color c1, float r1, Color c2, float r2, Color c3, float r3)
        {
            return ((c1 * r1) + (c2 * r2) + (c3 * r3)) / (r1 + r2 + r3);
        }

        public static Color blend(this Color c1, float r1, Color c2, float r2)
        {
            return ((c1 * r1) + (c2 * r2)) / (r1 + r2);
        }

        public static Color fancyBlend(this Color c1, Color c2, float ratio)
        {
            var hsb1 = c1.toHSB();
            var hsb2 = c2.toHSB();
            var fhsb = (1f - ratio) * hsb1 + ratio * hsb2;
            return Color.HSVToRGB(fhsb.x, fhsb.y, fhsb.z);
        }

        public static Color32 AdjustBrightness(this Color32 color, float diff, float minBrightness = 0f, float maxBrightness = 255f)
        {
            float hue, saturation, value;

            // Normalize 0-255 to 0-1
            float min = minBrightness / 255f;
            float max = maxBrightness / 255f;
            diff /= 255f;

            Color.RGBToHSV(color, out hue, out saturation, out value);
            diff = Mathf.Clamp(diff, -1, 1);
            value += diff;
            value = Mathf.Clamp(value, min, max);
            Color32 newColor = Color.HSVToRGB(hue, saturation, value);

            return newColor;
        }

    }

}
