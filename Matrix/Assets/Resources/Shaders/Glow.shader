// Upgrade NOTE: replaced 'mul(UNITY_MATRIX_MVP,*)' with 'UnityObjectToClipPos(*)'

Shader "Custom/Glow"
{
	Properties
	{
		_MainTex("Main Texture", 2D) = "white" {}
		_SceneTex("Scene Texture", 2D) = "white" {}
	}
	SubShader
	{
	//Blend SrcAlpha OneMinusSrcAlpha
		Pass
		{
			CGPROGRAM

			sampler2D _MainTex;
			sampler2D _SceneTex;
			float2 _MainTex_TexelSize;
			#pragma vertex vert
			#pragma fragment frag
			#pragma only_renderers d3d11
			#include "UnityCG.cginc"

			struct v2f
			{
				float4 pos : SV_POSITION;
				float2 uvs : TEXCOORD0;
			};

			v2f vert(appdata_base v)
			{
				v2f o;
				o.pos = UnityObjectToClipPos(v.vertex);
				o.uvs = o.pos.xy / 2.0f + 0.5;
				return o;
			}

			half frag(v2f i) : COLOR
			{
				// return tex2D(_MainTex,float2(i.uvs.x, 1-i.uvs.y));
				int numIter = 20;
				float TX_x = _MainTex_TexelSize.x;
				//float TX_y = _MainTex_TexelSize.y;
				float colorIntensity = 0.0f;
				
				if (tex2D(_MainTex, float2(i.uvs.x, 1 - i.uvs.y)).r > 0) {
					//discard;
					return tex2D(_SceneTex, float2(i.uvs.x, 1 - i.uvs.y));
				}

				for (int k = 0; k < numIter; k++) {
					/*colorIntensity += tex2D(_MainTex, float2(i.uvs.x,1 - i.uvs.y) +
						float2((k - numIter / 2)*TX_x, (j - numIter / 2)*TX_y)).r;*/
					// horizontal blur
					colorIntensity += tex2D(_MainTex, float2(i.uvs.x, 1 - i.uvs.y) +
						float2((k - numIter / 2)*TX_x, 0)).r / numIter;
				}

				// return colorIntensity*half4(0, 1, 1, 1);
				return colorIntensity;
			}

				ENDCG
		}

		GrabPass{}

		Pass
		{
			CGPROGRAM

			sampler2D _MainTex;
			sampler2D _SceneTex;
			sampler2D _GrabTexture;
			float2 _GrabTexture_TexelSize;

#pragma vertex vert
#pragma fragment frag
#include "UnityCG.cginc"
			struct v2f
			{
				float4 pos : SV_POSITION;
				float2 uvs : TEXCOORD0;
			};

			v2f vert(appdata_base v)
			{
				v2f o;
				o.pos = UnityObjectToClipPos(v.vertex);
				o.uvs = o.pos.xy / 2 + 0.5f;
				return o;
			}

			half4 frag(v2f i) : COLOR
			{
				// return tex2D(_MainTex,float2(i.uvs.x, 1-i.uvs.y));
				int numIter = 20;
				float TX_y = _GrabTexture_TexelSize.y;
				half colorIntensity = 0;

				if (tex2D(_MainTex, float2(i.uvs.x, 1-i.uvs.y)).r > 0) {
					//discard;
					return tex2D(_SceneTex, float2(i.uvs.x, 1 - i.uvs.y));
				}

				for (int j = 0; j < numIter; j++) {
					/*colorIntensity += tex2D(_MainTex, float2(i.uvs.x,1 - i.uvs.y) +
					float2((k - numIter / 2)*TX_x, (j - numIter / 2)*TX_y)).r;*/
					// vertical blur
					colorIntensity += tex2D(_GrabTexture, float2(i.uvs.x, 1 - i.uvs.y) +
						float2(0,(j - numIter / 2)*TX_y)).r / numIter;
				}

				// return colorIntensity*half4(0, 1, 1, 1);
				return colorIntensity * half4(0, 1, 1, 1) + (1 - colorIntensity)*tex2D(_SceneTex, float2(i.uvs.x, 1 - i.uvs.y));
			}
			ENDCG
		}
	}
}