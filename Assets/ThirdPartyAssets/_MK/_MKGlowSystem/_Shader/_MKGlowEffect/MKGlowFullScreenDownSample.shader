// Upgrade NOTE: replaced 'mul(UNITY_MATRIX_MVP,*)' with 'UnityObjectToClipPos(*)'

Shader "Hidden/MKGlowFullScreenDownSample" 
{
	Properties 
	{
		_Color ("Color", color) = (1,1,1,0)
		_MainTex ("", 2D) = "white" {}
	}
		Subshader 
		{ 
			ZTest LEqual 
			Cull Off 
			ZWrite Off 
			Fog { Mode Off }
			Pass
			{
				CGPROGRAM
				#include "UnityCG.cginc"
				#pragma vertex vert
				#pragma fragment frag
				#pragma fragmentoption ARB_precision_hint_fastest

				uniform sampler2D _MainTex : register (s0);
				uniform fixed4 _Color;

				struct v2f 
				{
					float4 pos : POSITION;
					float4 uv[4] : TEXCOORD0;
				};

				uniform fixed4 _MainTex_TexelSize;

				v2f vert (appdata_img v)
				{
					v2f o;
					o.pos = UnityObjectToClipPos (v.vertex);
					float4 uv;
					uv.xy = MultiplyUV (UNITY_MATRIX_TEXTURE0, v.texcoord);
					uv.zw = 0;
					float offX = _MainTex_TexelSize.x;
					float offY = _MainTex_TexelSize.y;
					
					o.uv[0] = uv + float4(-offX,-offY,0,1);
					o.uv[1] = uv + float4( offX,-offY,0,1);
					o.uv[2] = uv + float4( offX, offY,0,1);
					o.uv[3] = uv + float4(-offX, offY,0,1);
					return o;
				}
				fixed4 frag( v2f i ) : SV_TARGET
				{
					fixed4 c;
					c  = tex2D( _MainTex, i.uv[0].xy );
					c += tex2D( _MainTex, i.uv[1].xy );
					c += tex2D( _MainTex, i.uv[2].xy );
					c += tex2D( _MainTex, i.uv[3].xy );
					c /= 4;
					c.rgb *= _Color.rgb;
					c.rgb *= (c.a + _Color.a);
					c.a = 0;
					return c;
				}
				ENDCG
			}
		}
	Fallback off

}
