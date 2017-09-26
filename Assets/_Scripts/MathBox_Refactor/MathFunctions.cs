/******************************************
* A library containing all math functions
* Define all math functions here
******************************************/

using UnityEngine;
using System.Collections;

namespace MathFunctions
{
    public class TwoVariableFunctions
    {
        public delegate float TwoVariableFunction(float x, float y);

        public enum TwoVarFnOptions :int
        {
            LINEAR = 0,
            EXPO,
            PARABOLA,
            SIN,
            COS,
            RIPPLE,
            HEMISPHERE
        }

        public static TwoVariableFunction[] twoVarFns =
        {
            Linear,
            Exponential,
            Parabola,
            Sine,
            Cosine,
            Ripple,
            Hemisphere
        };

        public static float Linear(float x, float y)
        {
            return x + y;
        }

        public static float Exponential(float x, float y)
        {
            return x * x / 3f;
        }

        public static float Parabola(float x, float y)
        {
            return 2f - Mathf.Pow(x/3f, 2f) * Mathf.Pow(y/3f, 2f);
        }

        public static float Sine(float x, float y)
        {
            return Mathf.Sin(x) + Mathf.Sin(y);
        }

        public static float Cosine(float x, float y)
        {
            return Mathf.Cos(x) + Mathf.Cos(y);
        }

        public static float Ripple(float x, float y)
        {
            float a = 5f;
            float b = 20f;
            float squareRadius = x * x + y * y;
            return Mathf.Sin(a * Mathf.PI * squareRadius) / (b * squareRadius);
        }

        public static float Hemisphere(float x, float y)
        {
            float r = 5f;
            if (r * r - x * x - y * y < 0)
            {
                return 0;
            }
            else
            {
                return Mathf.Sqrt(r * r - x * x - y * y);
            }
        }
    }

    public class VectorFieldFunctions
    {
        public delegate Vector3 VectorFieldFunction(Vector3 v);

        public enum VecFieldFnOptions : int
        {
            LINEAR = 0,
            TORNADO,
            RECYCLE,
            SQUARE,
            CIRCLE,
            GRAVITY
        }

        public static VectorFieldFunction[] vecFieldFns =
        {
            Linear,
            Tornado,
            Recycling,
            Square,
            Circle,
            EarthGravity
        };

        public static Vector3 Linear(Vector3 v)
        {
            return v * 3f;
        }

        public static Vector3 Tornado(Vector3 v)
        {
            //float x0 = (v.y == 0) ? v.z/Mathf.Epsilon : v.z / v.y;
            float x0 = v.z;
            float y0 = 0;
            //float z0 = -x / y;
            //float z0 = (v.y == 0) ? -v.x / Mathf.Epsilon : -v.x / v.y;
            float z0 = -v.x;
            return new Vector3(x0, y0, z0);
        }

        public static Vector3 Recycling(Vector3 v)
        {
            float x0 = v.y * v.z;
            float y0 = v.x * v.z;
            float z0 = v.x * v.y;
            return new Vector3(x0, y0, z0);
        }

        public static Vector3 Square(Vector3 v)
        {
            float x = v.x * v.x;
            float y = v.y * v.y;
            float z = v.z * v.z;
            return new Vector3(x, y, z);
        }

        public static Vector3 Circle(Vector3 v)
        {
            float x = -v.y;
            float y = -v.z;
            float z = v.x;
            return new Vector3(x, y, z);
        }

        public static Vector3 EarthGravity(Vector3 v)
        {
            float r = 4f;
            Vector3 g = v.normalized * -9.8f;
            if (v.magnitude > r)
            {
                return g * Mathf.Pow(r / (r + v.magnitude), 2f);
            }
            else
            {
                return Vector3.zero;
            }
        }
    }

    public class ParametrizedEquations
    {
        public delegate Vector3 ParametrizedEquation(float u, float v);

        public enum ParametrizationOptions : int
        {
            SPIRAL,
            SPHERE,
            TORUS,
            CIRCLE,
            MOBIUS,
            BREATHER,
            HYPERBOLOID,
            HYPERBOLICPARABALOID,
            KLEINBOTTLE,
            RIPPLE,
            CARDIOID,
            DINIS,
            PATH,
            CLOSE_SURFACE,
            PATH_PROJ,
            AREA_PROJ
        }

        public static ParametrizedEquation[] paramFunctions =
        {
            Spiral,
            Sphere,
            Torus,
            Circle,
            MobiusStrip,
            BreatherSurface,
            Hyperboloid,
            HyperbolicParaboloid,
            KleinBottle,
            Ripple,
            Cardioid,
            DinisSurface,
            Path,
            CloseArea,
            PathProjection,
            CloseAreaProjection
        };

        public static Vector3 Spiral(float u, float v)
        {
            float x = u * Mathf.Cos(v);
            float y = u * Mathf.Sin(v);
            float z = v;
            return new Vector3(x, z, y);
        }

        public static Vector3 Sphere(float u, float v)
        {
            float radius = 5.0f;
            float x = radius * Mathf.Cos(u) * Mathf.Sin(v);
            float y = radius * Mathf.Cos(u) * Mathf.Cos(v);
            float z = radius * Mathf.Sin(u);
            return new Vector3(x, z, y);
        }

        public static Vector3 Torus(float u, float v)
        {
            float a = 2f;
            float c = 5f;
            float x = (c + a * Mathf.Cos(v)) * Mathf.Cos(u);
            float y = (c + a * Mathf.Cos(v)) * Mathf.Sin(u);
            float z = a * Mathf.Sin(v);

            return new Vector3(x, z, y);
        }

        public static Vector3 Circle(float u, float v)
        {
            //float radius = 5f;
            float x = u * Mathf.Cos(v);
            float y = u * Mathf.Sin(v);
            float z = u * u;
            return new Vector3(x, z, y);
        }

        public static Vector3 MobiusStrip(float u, float v)
        {
            float aa = 5f;
            float x = aa * (Mathf.Cos(v) + u * Mathf.Cos(v / 2) * Mathf.Cos(v));
            float y = aa * (Mathf.Sin(v) + u * Mathf.Cos(v / 2) * Mathf.Sin(v));
            float z = aa * u * Mathf.Sin(v / 2f);
            return new Vector3(x, z, y);
        }

        public static Vector3 BreatherSurface(float u, float v)
        {
            float a = 0.5f;
            float _a = 1f - a * a;

            float x = -u + (2f * (_a) * (float)System.Math.Cosh(a * u) * (float)System.Math.Sinh(a * u)) / (a * (_a) * Mathf.Pow((float)System.Math.Cosh(a * u), 2f) + a * a * Mathf.Pow(Mathf.Sin(_a * v), 2f));

            float y = (2f * Mathf.Sqrt(_a) * (float)System.Math.Cosh(a * u) * (-Mathf.Sqrt(_a) * Mathf.Cos(v) * Mathf.Cos(Mathf.Sqrt(_a) * v) - Mathf.Sin(v) * Mathf.Sin(Mathf.Sqrt(_a) * v))) /
                (a * (_a * (float)System.Math.Cosh(a * u) * (float)System.Math.Cosh(a * u) + a * a * Mathf.Sin(Mathf.Sqrt(_a) * v) * Mathf.Sin(Mathf.Sqrt(_a) * v)));

            float z = (2f * Mathf.Sqrt(_a) * (float)System.Math.Cosh(a * u) * (-Mathf.Sqrt(_a) * Mathf.Sin(v) * Mathf.Cos(Mathf.Sqrt(_a) * v) + Mathf.Cos(v) * Mathf.Sin(Mathf.Sqrt(_a) * v))) /
                (a * (_a * (float)System.Math.Cosh(a * u) * (float)System.Math.Cosh(a * u) + a * a * Mathf.Sin(Mathf.Sqrt(_a) * v) * Mathf.Sin(Mathf.Sqrt(_a) * v)));

            return new Vector3(x, z, y);
        }
        public static Vector3 Hyperboloid(float u, float v)
        {
            float x = 3 * (float)System.Math.Cosh(u) * Mathf.Cos(v);
            float y = 3 * (float)System.Math.Cosh(u) * Mathf.Sin(v);
            float z = 3 * (float)System.Math.Sinh(u);

            return new Vector3(x, z, y);
        }
        public static Vector3 HyperbolicParaboloid(float u, float v)
        {
            float x = u;
            float y = v;
            float z = u * v;

            return new Vector3(x, z, y);
        }
        public static Vector3 KleinBottle(float u, float v)
        {
            /*
            float x = Mathf.Cos(u) * (Mathf.Cos(u / 2) * (Mathf.Sqrt(2) + Mathf.Cos(v)) + Mathf.Sin(u / 2) * Mathf.Sin(v) * Mathf.Cos(v));
            float y = Mathf.Sin(u) * (Mathf.Cos(u / 2) * (Mathf.Sqrt(2) + Mathf.Cos(v)) + Mathf.Sin(u / 2) * Mathf.Sin(v) * Mathf.Cos(v));
            float z = -Mathf.Sin(u / 2) * (Mathf.Sqrt(2) + Mathf.Cos(v)) + Mathf.Cos(u / 2) * Mathf.Sin(v) * Mathf.Cos(v);
            */
            float x = -2f / 15f * Mathf.Cos(u) * (3f * Mathf.Cos(v) - 30f * Mathf.Sin(u) + 90f * Mathf.Pow(Mathf.Cos(u), 4f) * Mathf.Sin(u) -
                60f * Mathf.Pow(Mathf.Cos(u), 6f) * Mathf.Sin(u) + 5f * Mathf.Cos(u) * Mathf.Cos(v) * Mathf.Sin(u));
            float y = -1f / 15f * Mathf.Sin(u) * (3f * Mathf.Cos(v) - 3f * Mathf.Pow(Mathf.Cos(u), 2f) * Mathf.Cos(v) - 48f * Mathf.Pow(Mathf.Cos(u), 4f) * Mathf.Cos(v) +
                48f * Mathf.Pow(Mathf.Cos(u), 6f) * Mathf.Cos(v) - 60f * Mathf.Sin(u) + 5f * Mathf.Cos(u) * Mathf.Cos(v) * Mathf.Sin(u) -
                5f * Mathf.Pow(Mathf.Cos(u), 3f) * Mathf.Cos(v) * Mathf.Sin(u) - 80f * Mathf.Pow(Mathf.Cos(u), 5f) * Mathf.Cos(v) * Mathf.Sin(u) +
                80f * Mathf.Pow(Mathf.Cos(u), 7f) * Mathf.Cos(v) * Mathf.Sin(u));
            float z = 2f / 15f * (3f + 5f * Mathf.Cos(u) * Mathf.Sin(u)) * Mathf.Sin(v);

            return new Vector3(x, z, y);
        }
        public static Vector3 Ripple(float u, float v)
        {
            float z = 1f / (Mathf.Sqrt(u * u + v * v) + .2f) * Mathf.Cos(5f * Mathf.Sqrt(u*u+v* v));

            return new Vector3 (u, z, v);
        }
        public static Vector3 Cardioid(float u, float v)
        {
            float x = 2f * Mathf.Cos(u) * (1f - Mathf.Cos(u)) * Mathf.Abs(Mathf.Cos(v));
            float y = 2f * Mathf.Sin(u) * (1f - Mathf.Cos(u)) * Mathf.Abs(Mathf.Cos(v));
            float z = 1f * Mathf.Sin(v) * Mathf.Cos(v);

            return new Vector3(x, z, y);
        }

        public static Vector3 DinisSurface(float u, float v)
        {
            float a = 1f;
            float b = .2f;
            float x = a * Mathf.Cos(u) * Mathf.Sin(v);
            float y = a * Mathf.Sin(u) * Mathf.Sin(v);
            float z = a * (Mathf.Cos(v) + Mathf.Log(Mathf.Tan(v / 2f))) + b * u;
            return new Vector3(x, z, y);
        }

        public static Vector3 Path(float u, float v)
        {
            float radius = 2f * Mathf.PI * 0.2f;
            float x = 2f * Mathf.Cos(u) * radius;
            float y = Mathf.Sin(u) * radius;
            float z = .25f * Mathf.Cos(10f * u) + 3f;
            return new Vector3(x, z, y);
        }

        public static Vector3 CloseArea(float u, float v)
        {
            float radius = 2f;
            float x = 2f * Mathf.Cos(u) * radius * v;
            float y = Mathf.Sin(u) * radius * v;
            float z = .25f * Mathf.Cos(10f * u) + 3f;
            return new Vector3(x, z, y);
        }

        public static Vector3 PathProjection(float u, float v)
        {
            float radius = 2f * Mathf.PI * 0.2f;
            float x = 2f * Mathf.Cos(u) * radius;
            float y = Mathf.Sin(u) * radius;
            float z = 0f;
            return new Vector3(x, z, y);
        }

        public static Vector3 CloseAreaProjection(float u , float v)
        {
            float radius = 2f;
            float x = 2f * Mathf.Cos(u) * radius * v;
            float y = Mathf.Sin(u) * radius * v;
            float z = 0f;
            return new Vector3(x, z, y);
        }
    }
}
