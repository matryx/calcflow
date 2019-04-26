// Upgrade NOTE: replaced 'mul(UNITY_MATRIX_MVP,*)' with 'UnityObjectToClipPos(*)'

Shader "Hidden/MKGlowCompose" 
{
	Properties 
	{
		_MainTex ("", 2D) = "Black" {}
		_GlowTex ("", 2D) = "Black" {}
	}
	Subshader 
	{
		ZTest off 
		Fog { Mode Off }
		//ColorMask RGB
		Cull back
		Lighting Off
		ZWrite Off
		
		Pass 
		{
			Blend One Zero
			CGPROGRAM
			#pragma vertex vert
			#pragma fragment frag
			#pragma fragmentoption ARB_precision_hint_fastest
			
			uniform sampler2D _MainTex;
			uniform float4 _GlowTex_TexelSize;
			struct Input
			{
				float4 texcoord : TEXCOORD0;
				float4 vertex : POSITION;
			};
			
			struct Output 
			{
				float4 pos : SV_POSITION;
				float2 uv : TEXCOORD0;
			};
			
			Output vert (Input i)
			{
				Output o;
				o.pos = UnityObjectToClipPos (i.vertex);
				o.uv = i.texcoord;
				return o;
			}

			fixed4 frag( Output i ) : SV_TARGET
			{	
			       i.uv.y = 1-i.uv.y;
				fixed4 g = tex2D( _MainTex, i.uv);
				return g;
			}
			ENDCG
		}
		
		Pass 
		{
			Blend One One
			CGPROGRAM
			#pragma vertex vert
			#pragma fragment frag
			#pragma fragmentoption ARB_precision_hint_fastest
			
			uniform sampler2D _GlowTex;
			uniform float4 _GlowTex_TexelSize;
			struct Input
			{
				float4 texcoord : TEXCOORD0;
				float4 vertex : POSITION;
			};
			
			struct Output 
			{
				float4 pos : SV_POSITION;
				float2 uv : TEXCOORD0;
			};
			
			Output vert (Input i)
			{
				Output o;
				o.pos = UnityObjectToClipPos (i.vertex);
				o.uv = i.texcoord;
				return o;
			}

			fixed4 frag( Output i ) : SV_TARGET
			{
				#if UNITY_UV_STARTS_AT_TOP
				if (_GlowTex_TexelSize.y < 0)
			       i.uv.y = 1-i.uv.y;
				#endif

				fixed4 g = tex2D(_GlowTex, i.uv);
				return g;
			}
			ENDCG
		}
	}
	Fallback off
}