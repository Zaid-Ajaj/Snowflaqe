module SampleCraftSchema

open System.IO
open Expecto
open Snowflaqe
open Snowflaqe.Types

let [<Literal>] typesFileName = "Types.fs"

let craftSchemaPath = Utilities.path [ Utilities.tests; "CraftSchema.json" ]
let craftSchema = File.ReadAllText craftSchemaPath
let craftSchemaV2Path = Utilities.path [ Utilities.tests; "./CraftSchemaV2.json" ]
let craftSchemaV2 = File.ReadAllText craftSchemaV2Path

let validQuery = """
query HomeConfig {
  HomeConfig: entries(site: "stephenHamiltonNew" type: ["Home"]){
    __typename
    ...on Home_Home_Entry {
      __typename
      HeroLink
  	  Grid {
        ... on Grid_row_BlockType {
          __typename
          typeHandle
        	items {
            url
            width
            height
            id
          }
        }
      }
    }
  }
  Sections: entries(site: "stephenHamiltonNew", type: "PortfolioSection") {
    __typename
    ... on PortfolioSection_portfolioSection_Entry {
      __typename
      siteTitle
      siteSubtitle
      Grid {
        ... on Grid_row_BlockType {
          __typename
          items {
            id
            width
            height
            url
          }
        }
      }
    }
  }
}
"""

let moreComplicatedQuery = """
query home {
  HomeConfig: entries(site: "stephenHamiltonNew" type: ["Home"]){
    typeHandle
    __typename
    ...on Home_Home_Entry {
      __typename
      HeroVideo: video {
        url
      }
  	  Grid {
        ... GridItems
      }
    }
  }

  StudioConfig: entries(site: "stephenHamiltonNew", section: "studioPage") {
    __typename
	  ... on studioPage_videoAbove_Entry {
      __typename
      title
      video {
        url
      }
      markdownBody
    }
    ... on studioPage_photoAbove_Entry {
      __typename
      title
      photo {
        url
      }
      markdownBody
    }
    ... on studioPage_photoGalleryAbove_Entry {
      __typename
      title
      photoGallery {
        url
      }
      markdownBody
    }
    ... on studioPage_layeredPhotoAbove_Entry {
      __typename
      title
      markdownBody
      layeredPhotos {
        __typename
				... on layeredPhotos_topLeftPhotoAnchor_BlockType {
          __typename
          image {
            url
          }
          width
          layer
          topAnchor
          leftAnchor
        }
        ... on layeredPhotos_topRightPhotoAnchor_BlockType {
          __typename
          image {
            url
          }
          width
          layer
          topAnchor
          rightAnchor
        }
        ... on layeredPhotos_bottomLeftPhotoAnchor_BlockType {
          __typename
          image {
            url
          }
          width
          layer
          bottomAnchor
          leftAnchor
        }
        ... on layeredPhotos_bottomRightPhotoAnchor_BlockType {
          __typename
          image {
            url
          }
          width
          layer
          bottomAnchor
          rightAnchor
        }
      }
    }
  }
  TeamMembers:entries(site: "stephenHamiltonNew", section: "teamMembers") {
    __typename
    ... on teamMembers_teamMembers_Entry {
      __typename
      id
      firstName
      lastName
      profilePhoto {
        url
      }
      profileParagraph
      portfolioSection {
        title
      }
      memberEmail
    }
  }
  AboutConfig: entries(site: "stephenHamiltonNew", section: "aboutPage"){
    __typename
		... on aboutPage_teamSection_Entry {
      __typename
      title
      teamMembers {
        __typename
        ... on teamMembers_teamRow_BlockType {
          __typename
          teamMembers {
            id
          }
        }
      }
    }
    ... on aboutPage_videoBelow_Entry {
      __typename
      title
      video {
    		url
      }
      markdownBody
    }
    ... on aboutPage_videoAbove_Entry {
      __typename
      title
      video {
        url
      }
      markdownBody
    }
    ... on aboutPage_photoAbove_Entry {
      __typename
      title
      photo {
        url
      }
      markdownBody
    }
  }
  ContactConfig: entries(site: "stephenHamiltonNew", section: "contactPage") {
    __typename
    ... on contactPage_contactPage_Entry {
      __typename
      contactAddress
      contactLatitude
      contactLongitude
      contactText
      emailContacts {
        id
      }
      salesContacts {
        id
      }
    }
  }
  Sections: entries(site: "stephenHamiltonNew", type: "PortfolioSection") {
    __typename
    ... on PortfolioSection_portfolioSection_Entry {
      __typename
      siteTitle
      siteSubtitle
      Grid {
        ...GridItems
      }
      displayOnMainPage
    }
  }
}
fragment GridItems on Grid_MatrixField {
  ... on Grid_row_BlockType {
    __typename
    typeHandle
    items {
      __typename
      kind
      id
      width
      height
      ... on portfolio_Asset {
        __typename
        videoWidth
        videoheight
        foodTypes {
          id
        }
      }
      ... on videoPortfolio_Asset {
        __typename
        videoWidth
        videoheight
        videoThumbnail {
          url
        }
      }
      ... on photoPortfolio_Asset {
        __typename
        specialUrl
      }
      mainUrl: url
      thumbnail: url @transform(transform: "thumbnail", immediately: false)
    }
  }
}
"""

let tests = testList "Craft schema" [
    test "query is valid" {
        match Query.parse validQuery, Schema.parse craftSchema with
        | Ok query, Ok schema ->
            let result = Query.validate query schema
            Expect.equal ValidationResult.Success result "Validation should succeed"
        | _ ->
            failwith "Unexpected"
    }

    test "generates valid code" {
        let schema = Schema.parse craftSchema
        let query = Query.parse validQuery

        match schema, query with
        | Ok schema, Ok query ->

            let name =
                Query.findOperationName query
                |> Option.defaultValue "DefaultQueryName"
                |> CodeGen.normalizeName

            let generated =
                let queryTypes = CodeGen.generateTypes name "ErrorType" query schema
                let ns = CodeGen.createQualifiedModule [ "Test"; name ] queryTypes
                let file = CodeGen.createFile typesFileName [ ns ]
                CodeGen.formatAst file typesFileName

            Expect.isNotEmpty generated "The code is generated correctly"

        | otherwise -> failwithf "%A" otherwise
    }


    test "generates valid code for more complicated query with collisions" {
      let schema = Schema.parse craftSchemaV2
      let query = Query.parse moreComplicatedQuery

      match schema, query with
      | Ok schema, Ok query ->

          let name =
              Query.findOperationName query
              |> Option.defaultValue "DefaultQueryName"
              |> CodeGen.normalizeName

          let generated =
              let queryTypes = CodeGen.generateTypes name "ErrorType" query schema
              let ns = CodeGen.createQualifiedModule [ "Test"; name ] queryTypes
              let file = CodeGen.createFile typesFileName [ ns ]
              CodeGen.formatAst file typesFileName

          Expect.isNotEmpty generated "The code is generated correctly"

      | otherwise -> failwithf "%A" otherwise
    }
]
