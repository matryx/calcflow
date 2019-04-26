// Upgrade NOTE: replaced 'mul(UNITY_MATRIX_MVP,*)' with 'UnityObjectToClipPos(*)'

Shader "Hidden/MKGlowBlur" 
{
	Properties 
	{
		_MainTex ("", 2D) = "black" {}
		_Color ("Color", color) = (1,1,1,0)
	}
	Subshader 
	{	
		ZTest LEqual 
		Fog { Mode Off }
		//ColorMask RGB
		Cull Off
		Lighting Off
		ZWrite Off

		Pass 
		{
			CGPROGRAM
			#pragma target 2.0
			#pragma vertex vert
			#pragma fragment frag
			#pragma fragmentoption ARB_precision_hint_fastest
			
			uniform sampler2D _MainTex : register (s0);
			uniform fixed4 _Color;
			
			struct Input
			{
				float4 texcoord : TEXCOORD0;
				float4 vertex : POSITION;
			};

			struct Output 
			{
				float4 pos : SV_POSITION;
				half4 uv[2] : TEXCOORD0;
			};

			uniform fixed4 _MainTex_TexelSize;
			uniform half2 _BlurOffsets;

			Output vert (Input v)
			{
				Output o;
				float offX = _MainTex_TexelSize.x * _BlurOffsets.x;
				float offY = _MainTex_TexelSize.y * _BlurOffsets.y;

				o.pos = UnityObjectToClipPos (v.vertex);
				float2 uv = v.texcoord.xy-float2(offX, offY);
			
				o.uv[0].xy = uv + float2( offX, offY);
				o.uv[0].zw = uv + float2(-offX, offY);
				o.uv[1].xy = uv + float2( offX,-offY);
				o.uv[1].zw = uv + float2(-offX,-offY);
				return o;
			}
			

			fixed4 frag( Output i ) : SV_TARGET
			{
				fixed4 c;
				c  = tex2D( _MainTex, i.uv[0].xy );
				c += tex2D( _MainTex, i.uv[0].zw );
				c += tex2D( _MainTex, i.uv[1].xy );
				c += tex2D( _MainTex, i.uv[1].zw );
				return c * _Color.a;
			}
			ENDCG
		}
	}
	Fallback off
}