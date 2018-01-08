using UnityEngine;
using System.Collections;

public class TMProExtension : RendererRSExtension 
{}


// using UnityEngine;
// using System.Collections;
// using VoxelBusters.RuntimeSerialization;
// using TMPro;

// [RuntimeSerializable]
// public class TMProExtension : ComponentRSExtension
// {
//     private const string koverflowMode                   ="overflowMode";
//     private const string kwordWrappingRatios             ="wordWrappingRatios";
//     private const string khorizontalMapping              ="horizontalMapping";
//     private const string kcharacterWidthAdjustment       ="characterWidthAdjustment";
//     private const string kparagraphSpacing               ="paragraphSpacing";
//     private const string klineSpacingAdjustment          ="lineSpacingAdjustment";
//     private const string klineSpacing                    ="lineSpacing";
//     private const string kenableWordWrapping             ="enableWordWrapping";
//     private const string klinkedTextComponent            ="linkedTextComponent";
//     private const string kignoreRectMaskCulling          ="ignoreRectMaskCulling";
//     private const string kenableKerning                  ="enableKerning";
//     private const string kextraPadding                   ="extraPadding";
//     private const string krichText                       ="richText";
//     private const string kparseCtrlCharacters            ="parseCtrlCharacters";
//     private const string kisOverlay                      ="isOverlay";
//     private const string kisOrthographic                 ="isOrthographic";
//     private const string kenableCulling                  ="enableCulling";
//     private const string kignoreVisibility               ="ignoreVisibility";
//     private const  tring kisLinkedTextComponent          ="isLinkedTextComponent";
//     private const string kcharacterSpacing               ="characterSpacing";
//     private const string kspriteAsset                    ="spriteAsset";
//     private const string kverticalMapping                ="verticalMapping";
//     private const string kfaceColor                      ="faceColor";
//     private const string koverrideColorTags              ="overrideColorTags";
//     private const string ktintAllSprites                 ="tintAllSprites";
//     private const string kcolorGradientPreset            ="colorGradientPreset";
//     private const string kcolorGradient                  ="colorGradient";
//     private const string kenableVertexGradient           ="enableVertexGradient";
//     private const string kalpha                          ="alpha";
//     private const string kcolor                          ="color";
//     private const string kfontMaterials                  ="fontMaterials";
//     private const string kfontMaterial                   ="fontMaterial";
//     private const string kalignment                      ="alignment";
//     private const string kfontSharedMaterials            ="fontSharedMaterials";
//     private const string kfont                           ="font";
//     private const string kisRightToLeftText              ="isRightToLeftText";
//     private const string ktext                           ="text";
//     private const string kfontSize                       ="fontSize";
//     private const string kfontWeight                     ="fontWeight";
//     private const string kenableAutoSizing               ="enableAutoSizing";
//     private const string kfontSizeMin                    ="fontSizeMin";
//     private const string kfontSizeMax                    ="fontSizeMax";
//     private const string kfontStyle                      ="fontStyle";
//     private const string kfontSharedMaterial             ="fontSharedMaterial";
//     private const string kmappingUvLineOffset            ="mappingUvLineOffset";
//     private const string koutlineWidth                   ="outlineWidth";
//     private const string krenderMode                     ="renderMode";
//     private const string kautoSizeTextContainer          ="autoSizeTextContainer";
//     private const string kisVolumetricText               ="isVolumetricText";
//     private const string kisUsingLegacyAnimationComponent="isUsingLegacyAnimationComponent";
//     private const string khavePropertiesChanged          ="havePropertiesChanged";
//     private const string koutlineColor                   ="outlineColor";
//     private const string kpageToDisplay                  ="pageToDisplay";
//     private const string kgeometrySortingOrder           ="geometrySortingOrder";
//     private const string kfirstVisibleCharacter          ="firstVisibleCharacter";
//     private const string kmaxVisibleCharacters           ="maxVisibleCharacters";
//     private const string kmargin                         ="margin";
//     private const string kmaxVisibleWords                ="maxVisibleWords";
//     private const string kmaxVisibleLines                ="maxVisibleLines";
//     private const string kuseMaxVisibleDescender         = "useMaxVisibleDescender";

//     public override void WriteSerializationData(object _object, RuntimeSerializationInfo _info)
//     {
//         TextMeshPro _textMeshPro = _object as TextMeshPro;

//         if (_textMeshPro == null)
//             return;

//         // Serialize base properties
//         base.WriteSerializationData(_object, _info);

//         // Serialize properties
//         _info.AddValue<TextOverflowModes>(koverflowMode, _textMeshPro.overflowMode);
//         _info.AddValue<float>(kwordWrappingRatios, _textMeshPro.wordWrappingRatios);
//         _info.AddValue<TextureMappingOptions>(khorizontalMapping, _textMeshPro.horizontalMapping);
//         _info.AddValue<float>(kcharacterWidthAdjustment, _textMeshPro.characterWidthAdjustment);
//         _info.AddValue<float>(kparagraphSpacing, _textMeshPro.paragraphSpacing);
//         _info.AddValue<float>(klineSpacingAdjustment, _textMeshPro.lineSpacingAdjustment);
//         _info.AddValue<float>(klineSpacing, _textMeshPro.lineSpacing);
//         _info.AddValue<bool>(kenableWordWrapping, _textMeshPro.enableWordWrapping);
//         _info.AddValue<TMP_Text>(klinkedTextComponent, _textMeshPro.linkedTextComponent);
//         _info.AddValue<bool>(kignoreRectMaskCulling, _textMeshPro.ignoreRectMaskCulling);
//         _info.AddValue<bool>(kenableKerning, _textMeshPro.enableKerning);
//         _info.AddValue<bool>(kextraPadding, _textMeshPro.extraPadding);
//         _info.AddValue<bool>(krichText, _textMeshPro.richText);
//         _info.AddValue<bool>(kparseCtrlCharacters, _textMeshPro.parseCtrlCharacters);
//         _info.AddValue<bool>(kisOverlay, _textMeshPro.isOverlay);
//         _info.AddValue<bool>(kisOrthographic, _textMeshPro.isOrthographic);
//         _info.AddValue<bool>(kenableCulling, _textMeshPro.enableCulling);
//         _info.AddValue<bool>(kignoreVisibility, _textMeshPro.ignoreVisibility);
//         _info.AddValue<float>(kwordSpacing, _textMeshPro.wordSpacing);
//         _info.AddValue<bool>(kisLinkedTextComponent, _textMeshPro.isLinkedTextComponent);
//         _info.AddValue<float>(kcharacterSpacing, _textMeshPro.characterSpacing);
//         _info.AddValue<TMP_SpriteAsset>(kspriteAsset, _textMeshPro.spriteAsset);
//         _info.AddValue<TextureMappingOptions>(kverticalMapping, _textMeshPro.verticalMapping);
//         _info.AddValue<Color32>(kfaceColor, _textMeshPro.faceColor);
//         _info.AddValue<bool>(koverrideColorTags, _textMeshPro.overrideColorTags);
//         _info.AddValue<bool>(ktintAllSprites, _textMeshPro.tintAllSprites);
//         _info.AddValue<TMP_ColorGradient>(kcolorGradientPreset, _textMeshPro.colorGradientPreset);
//         _info.AddValue<VertexGradient>(kcolorGradient, _textMeshPro.colorGradient);
//         _info.AddValue<bool>(kenableVertexGradient, _textMeshPro.enableVertexGradient);
//         _info.AddValue<float>(kalpha, _textMeshPro.alpha);
//         _info.AddValue<Color>(kcolor, _textMeshPro.color);
//         _info.AddValue<Material[]>(kfontMaterials, _textMeshPro.fontMaterials);
//         _info.AddValue<Material>(kfontMaterial, _textMeshPro.fontMaterial);
//         _info.AddValue<TextAlignmentOptions>(kalignment, _textMeshPro.alignment);
//         _info.AddValue<Material[]>(kfontSharedMaterials, _textMeshPro.fontSharedMaterials);
//         _info.AddValue<TMP_FontAsset>(kfont, _textMeshPro.font);
//         _info.AddValue<bool>(kisRightToLeftText, _textMeshPro.isRightToLeftText);
//         _info.AddValue<string>(ktext, _textMeshPro.text);
//         _info.AddValue<float>(kfontSize, _textMeshPro.fontSize);
//         _info.AddValue<int>(kfontWeight, _textMeshPro.fontWeight);
//         _info.AddValue<bool>(kenableAutoSizing, _textMeshPro.enableAutoSizing);
//         _info.AddValue<float>(kfontSizeMin, _textMeshPro.fontSizeMin);
//         _info.AddValue<float>(kfontSizeMax, _textMeshPro.fontSizeMax);
//         _info.AddValue<FontStyles>(kfontStyle, _textMeshPro.fontStyle);
//         _info.AddValue<Material>(kfontSharedMaterial, _textMeshPro.fontSharedMaterial);
//         _info.AddValue<float>(kmappingUvLineOffset, _textMeshPro.mappingUvLineOffset);
//         _info.AddValue<float>(koutlineWidth, _textMeshPro.outlineWidth);
//         _info.AddValue<TextRenderFlags>(krenderMode, _textMeshPro.renderMode);
//         _info.AddValue<bool>(kautoSizeTextContainer, _textMeshPro.autoSizeTextContainer);
//         _info.AddValue<bool>(kisVolumetricText, _textMeshPro.isVolumetricText);
//         _info.AddValue<bool>(kisUsingLegacyAnimationComponent, _textMeshPro.isUsingLegacyAnimationComponent);
//         _info.AddValue<bool>(khavePropertiesChanged, _textMeshPro.havePropertiesChanged);
//         _info.AddValue<Color32>(koutlineColor, _textMeshPro.outlineColor);
//         _info.AddValue<int>(kpageToDisplay, _textMeshPro.pageToDisplay);
//         _info.AddValue<VertexSortingOrder>(kgeometrySortingOrder, _textMeshPro.geometrySortingOrder);
//         _info.AddValue<int>(kfirstVisibleCharacter, _textMeshPro.firstVisibleCharacter);
//         _info.AddValue<int>(kmaxVisibleCharacters, _textMeshPro.maxVisibleCharacters);
//         _info.AddValue<Vector4>(kmargin, _textMeshPro.margin);
//         _info.AddValue<int>(kmaxVisibleWords, _textMeshPro.maxVisibleWords);
//         _info.AddValue<int>(kmaxVisibleLines, _textMeshPro.maxVisibleLines);
//         _info.AddValue<bool>(kuseMaxVisibleDescender, _textMeshPro.useMaxVisibleDescender);
//     }

//     public override object ReadSerializationData(object _object, RuntimeSerializationInfo _info)
//     {

//         // Deserialize base properties
//         TextMeshPro _textMeshPro = base.ReadSerializationData(_object, _info) as TextMeshPro;

//         if (_textMeshPro == null)
//             return null;

//         // Deserialize properties
//         _textMeshPro.overflowMode = _info.GetValue<TextOverflowModes>(koverflowMode);
//         _textMeshPro.wordWrappingRatios = _info.GetValue<float>(kwordWrappingRatios);
//         _textMeshPro.horizontalMapping = _info.GetValue<TextureMappingOptions>(khorizontalMapping);
//         _textMeshPro.characterWidthAdjustment = _info.GetValue<float>(kcharacterWidthAdjustment);
//         _textMeshPro.paragraphSpacing = _info.GetValue<float>(kparagraphSpacing);
//         _textMeshPro.lineSpacingAdjustment = _info.GetValue<float>(klineSpacingAdjustment);
//         _textMeshPro.lineSpacing = _info.GetValue<float>(klineSpacing);
//         _textMeshPro.enableWordWrapping = _info.GetValue<bool>(kenableWordWrapping);
//         _textMeshPro.linkedTextComponent = _info.GetValue<TMP_Text>(klinkedTextComponent);
//         _textMeshPro.ignoreRectMaskCulling = _info.GetValue<bool>(kignoreRectMaskCulling);
//         _textMeshPro.enableKerning = _info.GetValue<bool>(kenableKerning);
//         _textMeshPro.extraPadding = _info.GetValue<bool>(kextraPadding);
//         _textMeshPro.richText = _info.GetValue<bool>(krichText);
//         _textMeshPro.parseCtrlCharacters = _info.GetValue<bool>(kparseCtrlCharacters);
//         _textMeshPro.isOverlay = _info.GetValue<bool>(kisOverlay);
//         _textMeshPro.isOrthographic = _info.GetValue<bool>(kisOrthographic);
//         _textMeshPro.enableCulling = _info.GetValue<bool>(kenableCulling);
//         _textMeshPro.ignoreVisibility = _info.GetValue<bool>(kignoreVisibility);
//         _textMeshPro.wordSpacing = _info.GetValue<float>(kwordSpacing);
//         _textMeshPro.isLinkedTextComponent = _info.GetValue<bool>(kisLinkedTextComponent);
//         _textMeshPro.characterSpacing = _info.GetValue<float>(kcharacterSpacing);
//         _textMeshPro.spriteAsset = _info.GetValue<TMP_SpriteAsset>(kspriteAsset);
//         _textMeshPro.verticalMapping = _info.GetValue<TextureMappingOptions>(kverticalMapping);
//         _textMeshPro.faceColor = _info.GetValue<Color32>(kfaceColor);
//         _textMeshPro.overrideColorTags = _info.GetValue<bool>(koverrideColorTags);
//         _textMeshPro.tintAllSprites = _info.GetValue<bool>(ktintAllSprites);
//         _textMeshPro.colorGradientPreset = _info.GetValue<TMP_ColorGradient>(kcolorGradientPreset);
//         _textMeshPro.colorGradient = _info.GetValue<VertexGradient>(kcolorGradient);
//         _textMeshPro.enableVertexGradient = _info.GetValue<bool>(kenableVertexGradient);
//         _textMeshPro.alpha = _info.GetValue<float>(kalpha);
//         _textMeshPro.color = _info.GetValue<Color>(kcolor);
//         _textMeshPro.fontMaterials = _info.GetValue<Material[]>(kfontMaterials);
//         _textMeshPro.fontMaterial = _info.GetValue<Material>(kfontMaterial);
//         _textMeshPro.alignment = _info.GetValue<TextAlignmentOptions>(kalignment);
//         _textMeshPro.fontSharedMaterials = _info.GetValue<Material[]>(kfontSharedMaterials);
//         _textMeshPro.font = _info.GetValue<TMP_FontAsset>(kfont);
//         _textMeshPro.isRightToLeftText = _info.GetValue<bool>(kisRightToLeftText);
//         _textMeshPro.text = _info.GetValue<string>(ktext);
//         _textMeshPro.fontSize = _info.GetValue<float>(kfontSize);
//         _textMeshPro.fontWeight = _info.GetValue<int>(kfontWeight);
//         _textMeshPro.enableAutoSizing = _info.GetValue<bool>(kenableAutoSizing);
//         _textMeshPro.fontSizeMin = _info.GetValue<float>(kfontSizeMin);
//         _textMeshPro.fontSizeMax = _info.GetValue<float>(kfontSizeMax);
//         _textMeshPro.fontStyle = _info.GetValue<FontStyles>(kfontStyle);
//         _textMeshPro.fontSharedMaterial = _info.GetValue<Material>(kfontSharedMaterial);
//         _textMeshPro.mappingUvLineOffset = _info.GetValue<float>(kmappingUvLineOffset);
//         _textMeshPro.outlineWidth = _info.GetValue<float>(koutlineWidth);
//         _textMeshPro.renderMode = _info.GetValue<TextRenderFlags>(krenderMode);
//         _textMeshPro.autoSizeTextContainer = _info.GetValue<bool>(kautoSizeTextContainer);
//         _textMeshPro.isVolumetricText = _info.GetValue<bool>(kisVolumetricText);
//         _textMeshPro.isUsingLegacyAnimationComponent = _info.GetValue<bool>(kisUsingLegacyAnimationComponent);
//         _textMeshPro.havePropertiesChanged = _info.GetValue<bool>(khavePropertiesChanged);
//         _textMeshPro.outlineColor = _info.GetValue<Color32>(koutlineColor);
//         _textMeshPro.pageToDisplay = _info.GetValue<int>(kpageToDisplay);
//         _textMeshPro.geometrySortingOrder = _info.GetValue<VertexSortingOrder>(kgeometrySortingOrder);
//         _textMeshPro.firstVisibleCharacter = _info.GetValue<int>(kfirstVisibleCharacter);
//         _textMeshPro.maxVisibleCharacters = _info.GetValue<int>(kmaxVisibleCharacters);
//         _textMeshPro.margin = _info.GetValue<Vector4>(kmargin);
//         _textMeshPro.maxVisibleWords = _info.GetValue<int>(kmaxVisibleWords);
//         _textMeshPro.maxVisibleLines = _info.GetValue<int>(kmaxVisibleLines);
//         _textMeshPro.useMaxVisibleDescender = _info.GetValue<bool>(kuseMaxVisibleDescender);

//         return _textMeshPro;
//     }

// }