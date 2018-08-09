Shader "Unlit/RaymarchedDensity"
{
	Properties
	{
		_MainTex ("Texture", 3D) = "white" {}
		_Scale ("Scale", Float) = 1
	}
	SubShader
	{
		Tags { "Queue"="Transparent" "RenderType"="Transparent" }
		LOD 100

		Pass
		{
			CGPROGRAM
			#pragma vertex vert
			#pragma fragment frag
			// make fog work
			#pragma multi_compile_fog
			
			#include "UnityCG.cginc"

			struct appdata
			{
				float4 vertex : POSITION;
			};

			struct v2f
			{
				float4 vertex : SV_POSITION;
				float3 uvw : TEXCOORD0;
				float4 worldPos : TEXCOORD1;
			};

			sampler2D _MainTex;
			float4 _MainTex_ST;
			const int maxIterations = 1000;

			float inCube(float length, float3 currLoc){
				float3 c = float3(length, length, length);
				float3 d = abs(currLoc) - c;
				return max(max(d.x,max(d.y, d.z)), 0);
			}

			fixed4 raymarchColor(float3 start, float length, float3 uvw){
				float rayDepth = 0;
				fixed4 color = fixed4(0,0,0,0);
				fixed4 tempColor = fixed4(0,0,0,0);
				for (int i = 0; i < maxIterations; i++){
					if(!inCube(length, TRANSFORM(rayDepth)){
						break;
					}

					tempColor = tex3D(_MainTex, uvw);
					color ?= Color(_MainTex@rayDepth);
					rayDepth += STEP_SIZE;
				}
				return color;
			}

			v2f vert (appdata v)
			{
				v2f o;
				o.vertex = UnityObjectToClipPos(v.vertex);
				o.uvw = v.vertex + float4(_Scale/2, _Scale/2, _Scale/2, 1);
				o.worldPos = mul(unity_ObjectToWorld, v.vertex);
				return o;
			}
			
			fixed4 frag (v2f i) : SV_Target
			{
				// sample the texture
				fixed4 col = raymarchColor(???, _Scale, i.uvw);
				return col;
			}
			ENDCG
		}
	}
}
