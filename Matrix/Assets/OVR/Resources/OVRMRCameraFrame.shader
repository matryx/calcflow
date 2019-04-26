// Upgrade NOTE: replaced 'mul(UNITY_MATRIX_MVP,*)' with 'UnityObjectToClipPos(*)'

Shader "Unlit/OVRMRCameraFrame"
{
	Properties
	{
		_Color("Main Color", Color) = (1,1,1,1)
		_MainTex("Main Texture", 2D) = "white" {}
		_Visible("Visible", Range(0.0,1.0)) = 1.0
		_ChromaAlphaCutoff("ChromaAlphaCutoff", Range(0.0,1.0)) = 0.01
		_ChromaToleranceA("ChromaToleranceA", Range(0.0,50.0)) = 5.0
		_ChromaToleranceB("ChromaToleranceB", Range(0.0,50.0)) = 5.0
		_ChromaShadows("ChromaShadows", Range(0.0,1.0)) = 0.02
	}
	SubShader
	{
		Tags { "Queue" = "Transparent" "IgnoreProjector" = "True" "RenderType" = "Transparent" }
		Blend SrcAlpha OneMinusSrcAlpha
		AlphaTest Greater .01
		Fog{ Mode Off }
		LOD 100
		Cull Off

		Pass
		{
			CGPROGRAM
			#pragma vertex vert
			#pragma fragment frag

			#include "UnityCG.cginc"

			struct appdata
			{
				float4 vertex : POSITION;
				float2 texcoord : TEXCOORD0;
			};

			struct v2f
			{
				float4 vertex : SV_POSITION;
				float2 texcoord : TEXCOORD0;
			};

			fixed4 _Color;
			sampler2D _MainTex;
			float4 _MainTex_ST;
			fixed  _Visible;
			float _ChromaAlphaCutoff;
			float _ChromaToleranceA;
			float _ChromaToleranceB;
			float _ChromaShadows;

			v2f vert (appdata v)
			{
				v2f o;
#if UNITY_VERSION >= 540
				o.vertex = UnityObjectToClipPos(v.vertex);
#else
				o.vertex = UnityObjectToClipPos(v.vertex);
#endif
				o.vertex *= _Visible;
				o.texcoord = TRANSFORM_TEX(float2(v.texcoord.x, 1.0-v.texcoord.y), _MainTex);
				return o;
			}

			float4 doChroma(float4 oldColor)
			{
				float a = _ChromaToleranceA;
				float b = _ChromaToleranceB;

				float alpha = (a * (oldColor.r + oldColor.b)) - (b * oldColor.g);

				if ((oldColor.r + oldColor.g + oldColor.b) / 3 < _ChromaShadows)
				{
					alpha = 1;
				}

				alpha = saturate(alpha);

				return float4(oldColor.r, oldColor.g, oldColor.b, alpha);
			}

      float4 doBleedTest(float4 oldColor)
			{
				float greenScalar = (oldColor.g - oldColor.r) / 25.0;
				greenScalar = clamp(greenScalar, 0.0, 1.0);
				float greenVal = min(oldColor.g, oldColor.b);
				return float4(oldColor.r, lerp(oldColor.g, greenVal, greenScalar), oldColor.b, oldColor.a);
			}

			fixed4 frag (v2f i) : SV_Target
			{
				float4 texel = tex2D(_MainTex, i.texcoord);
				float4 col = _Color * texel;
				float4 newColor = doChroma(col);
				if (newColor.a < _ChromaAlphaCutoff)
				{
					discard;
				}
				newColor = doBleedTest(newColor);
				return newColor;
			}
			ENDCG
		}
	}
}
