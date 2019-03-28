import Foundation
import SwiftSyntax

class Parser: SyntaxVisitor {
    private(set) var tokens: [TokenSyntax] = []
    private var index = 0
    private(set) var currentToken: TokenSyntax!
    

    // MARK: Practice 1

    override func visit(_ token: TokenSyntax) {
        print("Parsing \(token.tokenKind)")
        print(token)
        tokens.append(token)
        
    }

    @discardableResult
    func read() -> TokenSyntax {
        currentToken = tokens[index]
        index = index + 1
        return currentToken
    }

    func peek(_ n: Int = 0) -> TokenSyntax {
        return tokens[index+n]
    }

    // MARK: Practice 2

    private func extractNumberLiteral(from token: TokenSyntax) -> Double? {
        switch token.tokenKind {
        case .integerLiteral(let value):
            return atof(value)
        case .floatingLiteral(let value):
            return atof(value)
        default:
            return nil
        }
        

    }

    func parseNumber() -> Node {
        guard let value = extractNumberLiteral(from: currentToken) else {
            fatalError("any number is expected")
        }
        read() // eat literal
        return NumberNode(value: value)
    }

    private func extractIdentifierLiteral(from token: TokenSyntax) -> String? {
        switch token.tokenKind {
        case .identifier(let value):
            return value
        default:
            return nil
        }
        
    }
    
    
    
    func parseIdentifierExpression() -> Node {
        guard let value = extractIdentifierLiteral(from: currentToken) else {
            fatalError("any number is expected")
        }
        read() // eat literal
        
        // if this is func call
        if(currentToken.tokenKind == .leftParen){
            read()
            var args = [CallExpressionNode.Argument]()
            while(currentToken.tokenKind != .rightParen){
                
                args.append(parseCallExpressionArgument())
                if(currentToken.tokenKind == .comma){
                    read()
                }
            }
            read()
            return CallExpressionNode(callee: value, arguments:args )
        }
        return VariableNode(identifier: value)
    }
    
    
    func parseCallExpressionArgument() -> CallExpressionNode.Argument{
        var label : String? = nil
        
        //:を先読み
        switch peek(1).tokenKind {
        //a:a
        case .colon:
            switch currentToken.tokenKind {
            case .identifier(let value):
                label = value
                read()
            default:
                print(currentToken.tokenKind)
                fatalError()
            }
            switch currentToken.tokenKind {
            case .colon:
                read()
            default:
                fatalError()
            }
        //a
        default:
            let value = parseExpression()!
            return CallExpressionNode.Argument(label: nil, value: value)
        }
        
        
        let value = parseExpression()!
        return CallExpressionNode.Argument(label: label, value: value)
    }
    // MARK: Practice 3

    func extractBinaryOperator(from token: TokenSyntax) -> BinaryExpressionNode.Operator? {
        switch token.tokenKind {
        case .spacedBinaryOperator(let value):
            return BinaryExpressionNode.Operator(rawValue: value)
        default:
            return nil
        }
    }

    private func parseBinaryOperatorRHS(expressionPrecedence: Int, lhs: Node?) -> Node? {
        var currentLHS: Node? = lhs
        while true {
            let binaryOperator = extractBinaryOperator(from: currentToken!)
            let operatorPrecedence = binaryOperator?.precedence ?? -1
            
            // Compare between nextOperator's precedences and current one
            if operatorPrecedence < expressionPrecedence {
                return currentLHS
            }
            
            read() // eat binary operator
            var rhs = parsePrimary()
            if rhs == nil {
                return nil
            }
            
            // If binOperator binds less tightly with RHS than the operator after RHS, let
            // the pending operator take RHS as its LHS.
            let nextPrecedence = extractBinaryOperator(from: currentToken)?.precedence ?? -1
            if (operatorPrecedence < nextPrecedence) {
                // Search next RHS from currentRHS
                // next precedence will be `operatorPrecedence + 1`
                rhs = parseBinaryOperatorRHS(expressionPrecedence: operatorPrecedence + 1, lhs: rhs)
                if rhs == nil {
                    return nil
                }
            }
            
            guard let nonOptionalRHS = rhs else {
                fatalError("rhs must be nonnull")
            }
            
            currentLHS = BinaryExpressionNode(binaryOperator!,
                                              lhs: currentLHS!,
                                              rhs: nonOptionalRHS)
        }
    }

    // MARK: Practice 4

    func parseFunctionDefinitionArgument() -> FunctionNode.Argument {
        
        var label : String? = nil
        var variableName : String? = nil
        // label[_ | a] variableNamea :
        switch currentToken.tokenKind {
        case .identifier(let value):
            label = value
            read()
        case .wildcardKeyword:
            label = nil
            read()
        default:
            print()
            fatalError()
        }
        
        switch currentToken.tokenKind {
        case .colon:
            read()
        case .identifier(let value):
            variableName = value
            read()
            if(currentToken.tokenKind == .colon){
                read()
            } else {
                fatalError()
            }
        default:
            fatalError()
        }
        // Double Typeは無視
        switch currentToken.tokenKind {
        case .identifier:
            read()
        default:
            fatalError()
        }
        
        if label != nil {
            return FunctionNode.Argument(label: label!, variableName: variableName!)
        } else {
            return FunctionNode.Argument(label: nil, variableName: variableName!)
        }
        
    }

    func parseFunctionDefinition() -> Node {
        var name: String
        var returnType: Type
        var body: Node
        var arguments = [FunctionNode.Argument]()
        // func NAME
        switch currentToken.tokenKind {
        case .funcKeyword:
            read()
        default:
            fatalError()
        }
        switch currentToken.tokenKind {
        case .identifier(let value):
            name = value
            read()
        default:
            fatalError()
        }
        
        //(label: label, label: label )
        switch currentToken.tokenKind {
        case .leftParen:
            read()
            while(currentToken.tokenKind != .rightParen){
                let arg = parseFunctionDefinitionArgument()
                arguments.append(arg);
                if(currentToken.tokenKind == .comma){
                    read()
                }
            }
            read()
        default:
            fatalError()
        }
        // -> Type
        switch currentToken.tokenKind {
        case .arrow:
            read()
        default:
            fatalError()
        }
        switch currentToken.tokenKind {
        case .identifier:
            returnType = Type.double
            read()
        default:
            print(currentToken.tokenKind)
            fatalError()
        }
        //{body}
        switch currentToken.tokenKind {
        case .leftBrace:
            read()
            body = parseExpression()!
        default:
            fatalError()
        }
        switch currentToken.tokenKind {
        case .rightBrace:
            read()
        default:
            print(currentToken.tokenKind)
            fatalError()
        }
        return FunctionNode(name: name, arguments: arguments, returnType: returnType, body: body)
    }

    // MARK: Practice 7

    func parseIfElse() -> Node {
        
        var condition : Node? = nil
        var thenb : Node? = nil
        var elseb : Node? = nil
        // if condition
        switch currentToken.tokenKind {
        case .ifKeyword:
            read()
            condition = parseExpression()!
        default:
            fatalError()
        }
        // {body}
        switch currentToken.tokenKind {
        case .leftBrace:
            read()
            while(currentToken.tokenKind != .rightBrace){
                thenb = parseExpression()!
            }
            read()
        default:
            print(currentToken.tokenKind)
            fatalError()
        }
        
        //else
        switch currentToken.tokenKind {
        case .elseKeyword:
            read()
            //{}
            switch currentToken.tokenKind {
            case .leftBrace:
                read()
                while(currentToken.tokenKind != .rightBrace){
                    elseb = parseExpression()!
                }
                read()
                return IfElseNode(condition: condition!, then: thenb!, else: elseb!)
            default:
                fatalError()
            }
            
        default:
            print("ifonly")
            return IfElseNode(condition: condition!, then: thenb!, else: nil)
        }
    }

    // PROBABLY WORKS WELL, TRUST ME

    func parse() -> [Node] {
        var nodes: [Node] = []
        read()
        while true {
            switch currentToken.tokenKind {
            case .eof:
                return nodes
            case .funcKeyword:
                let node = parseFunctionDefinition()
                nodes.append(node)
            default:
                if let node = parseTopLevelExpression() {
                    nodes.append(node)
                    break
                } else {
                    read()
                }
            }
        }
        return nodes
    }

    private func parsePrimary() -> Node? {
        switch currentToken.tokenKind {
        case .identifier:
            return parseIdentifierExpression()
        case .integerLiteral, .floatingLiteral:
            return parseNumber()
        case .leftParen:
            return parseParen()
        case .funcKeyword:
            return parseFunctionDefinition()
        case .returnKeyword:
            return parseReturn()
        case .ifKeyword:
            return parseIfElse()
        case .eof:
            return nil
        default:
            fatalError("Unexpected token \(currentToken.tokenKind) \(currentToken.text)")
        }
        return nil
    }

    func parseExpression() -> Node? {
        guard let lhs = parsePrimary() else {
            return nil
        }
        return parseBinaryOperatorRHS(expressionPrecedence: 0, lhs: lhs)
    }

    private func parseReturn() -> Node {
        guard case .returnKeyword = currentToken.tokenKind else {
            fatalError("returnKeyword is expected but received \(currentToken.tokenKind)")
        }
        read() // eat return
        if let expression = parseExpression() {
            return ReturnNode(body: expression)
        } else {
            // return nothing
            return ReturnNode(body: nil)
        }
    }

    private func parseParen() -> Node? {
        read() // eat (
        guard let v = parseExpression() else {
            return nil
        }

        guard case .rightParen = currentToken.tokenKind else {
                fatalError("expected ')'")
        }
        read() // eat )

        return v
    }

    private func parseTopLevelExpression() -> Node? {
        if let expression = parseExpression() {
            // we treat top level expressions as anonymous functions
            let anonymousPrototype = FunctionNode(name: "main", arguments: [], returnType: .int, body: expression)
            return anonymousPrototype
        }
        return nil
    }
}

private extension BinaryExpressionNode.Operator {
    var precedence: Int {
        switch self {
        case .lessThan, .greaterThan : return 0
        case .addition, .subtraction: return 20
        case .multication, .division: return 40
        
        }
    }
}
