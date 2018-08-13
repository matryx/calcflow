Shader "RaymarchedDensity"
{
	Properties
	{
		_VolumeTex ("Texture", 3D) = "white" {}
		_Scale ("Scale", Float) = 1
		_MaxSteps("Step Limit", Float)=64
	}
	SubShader
	{
		Tags { "Queue"="Transparent" "RenderType"="Transparent" }
		LOD 100
		ZWrite Off
		Blend SrcAlpha OneMinusSrcAlpha

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
				//float3 uvw : TEXCOORD0;
				float4 worldPos : TEXCOORD1;
				float4 localPos : TEXCOORD2;
			};

			sampler3D _VolumeTex;
			float4 _MainTex_ST;
			float _Scale;
			int _MaxSteps;

			float inCube(float length, float3 currLoc){
				float3 c = float3(length, length, length);
				float3 d = abs(currLoc) - c;
				return max(max(d.x,max(d.y, d.z)), 0);
			}

			bool inSphere(float rad, float3 currLoc){
				return bool(max(length(currLoc)-rad, 0));
			}

			inline fixed4 getTexColor (float3 uvw){
				return tex3D(_VolumeTex, uvw);
			}

			fixed4 raymarchColor(float4 start, float3 dir){
				float rayDepth = 0;
				fixed4 color = fixed4(0,0,0,0);
				fixed4 tempColor = fixed4(0,0,0,0);
				int shifter = 0;
				[loop]
				for (int i = 0; i < _MaxSteps; i++){
					// if(!inCube(_Scale, start+rayDepth*dir)){
					// 	break;
					// }
					if(inSphere(_Scale, start.xyz+rayDepth*dir)){
						break;
					}
					tempColor = getTexColor((start.xyz+rayDepth*dir)+.5);
					//tempColor.a *= 1;
					tempColor.rgb *= tempColor.a;
					color = (1-color.a) * tempColor + color;
					color = fixed4(1.08*color.rgb,color.a);
					//shifter = step(color.a, tempColor.a);
					//color = (color+(tempColor*shifter))/(1+(1*tempColor));
					rayDepth += .025;
				}
				return color;
			}

			v2f vert (appdata v)
			{
				v2f o;
				o.vertex = UnityObjectToClipPos(v.vertex);
				//o.uvw = v.vertex.xyz+.5;
				//o.uvw = float3(v.vertex.x + _Scale/2, v.vertex.y + _Scale/2, v.vertex.z + _Scale/2);
				o.worldPos = mul(unity_ObjectToWorld, v.vertex);
				//o.uvw = o.worldPos.xyz*.5+.5;
				o.localPos = v.vertex;
				return o;
			}
			
			fixed4 frag (v2f i) : SV_Target
			{
				// sample the texture
				float3 worldDir = i.worldPos - _WorldSpaceCameraPos;
				float3 localDir = normalize (mul(unity_WorldToObject, worldDir));
				fixed4 col = raymarchColor(i.localPos, localDir);
				//fixed4 col = tex3D(_VolumeTex, i.uvw);
				return col;
			}
			ENDCG
		}
	}
}
