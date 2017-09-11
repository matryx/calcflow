// Upgrade NOTE: replaced 'mul(UNITY_MATRIX_MVP,*)' with 'UnityObjectToClipPos(*)'

Shader "MK/MKGlow/Toon/Basic" 
{
	Properties 
	{
		_Color ("Main Color", Color) = (.5,.5,.5,1)
		_MainTex ("Base (RGB)", 2D) = "white" {}
		_ToonShade ("ToonShader Cubemap(RGB)", CUBE) = "" {}
		
		_MKGlowColor ("Glow Color", Color) = (1,1,1,1)
		_MKGlowPower ("Glow Power", Range(0.0,5.0)) = 2.5
		_MKGlowTex ("Glow Texture", 2D) = "black" {}
		_MKGlowTexColor ("Glow Texture Color", Color) = (1,1,1,1)
		_MKGlowTexStrength ("Glow Texture Strength ", Range(0.0,10.0)) = 1.0
		_MKGlowOffSet ("Glow Width ", Range(0.0,0.075)) = 0.0
	}


	SubShader {
		Tags { "RenderType"="MKGlow" }
		Pass {
			Name "BASE"
			Cull Off
			
			CGPROGRAM
			#pragma vertex vert
			#pragma fragment frag
			#pragma fragmentoption ARB_precision_hint_fastest 

			#include "UnityCG.cginc"

			sampler2D _MainTex;
			samplerCUBE _ToonShade;
			float4 _MainTex_ST;
			float4 _Color;
			
			sampler2D _MKGlowTex;
			half _MKGlowTexStrength;
			fixed4 _MKGlowTexColor;

			struct appdata 
			{
				float4 vertex : POSITION;
				float2 texcoord : TEXCOORD0;
				float3 normal : NORMAL;
			};
			
			struct v2f {
				float4 pos : POSITION;
				float2 texcoord : TEXCOORD0;
				float3 cubenormal : TEXCOORD1;
			};

			v2f vert (appdata v)
			{
				v2f o;
				o.pos = UnityObjectToClipPos (v.vertex);
				o.texcoord = TRANSFORM_TEX(v.texcoord, _MainTex);
				o.cubenormal = mul (UNITY_MATRIX_MV, float4(v.normal,0));
				return o;
			}

			float4 frag (v2f i) : COLOR
			{
				float4 col = _Color * tex2D(_MainTex, i.texcoord);
				fixed4 d = tex2D(_MKGlowTex, i.texcoord) * _MKGlowTexColor;
				col.rgb += (d * _MKGlowTexStrength);
				float4 cube = texCUBE(_ToonShade, i.cubenormal);
				return float4(2.0f * cube.rgb * col.rgb, col.a);
			}
			ENDCG			
		}
	} 

	SubShader {
		Tags { "RenderType"="Opaque" }
		Pass {
			Name "BASE"
			Cull Off
			SetTexture [_MainTex] {
				constantColor [_Color]
				Combine texture * constant
			} 
			SetTexture [_ToonShade] {
				combine texture * previous DOUBLE, previous
			}
		}
	} 
	
	Fallback "VertexLit"
}